module Sandbox exposing (main)

import Browser
import Browser.Events
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Json.Decode as D
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Shader)


type alias Model =
    { theta : Float
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    }


getDrag model =
    model.dragger
        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
        |> Maybe.withDefault model.drag


initModel : Model
initModel =
    { theta = 0
    , dragger = Nothing
    , drag = vec2 0 0
    }


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2


main : Program () Model Msg
main =
    Browser.document
        { init = always ( initModel, Cmd.none )
        , view = doc
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        vectorDecoder : D.Decoder Vec2
        vectorDecoder =
            D.map2 Vec2.vec2
                (D.field "x" D.float)
                (D.field "y" D.float)

        drags =
            case model.dragger of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (vectorDecoder |> D.map Drag)
                        , Browser.Events.onMouseUp (vectorDecoder |> D.map DragEnd)
                        ]

                Nothing ->
                    Browser.Events.onMouseDown (vectorDecoder |> D.map DragStart)
    in
    Sub.batch
        [ drags
        , Browser.Events.onAnimationFrameDelta Animate
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }, Cmd.none )

        DragStart pos ->
            ( { model | dragger = Just { from = pos, to = pos } }, Cmd.none )

        Drag pos ->
            ( { model
                | dragger = Maybe.map (\drag -> { drag | to = pos }) model.dragger
              }
            , Cmd.none
            )

        DragEnd pos ->
            ( { model
                | dragger = Nothing
                , drag =
                    model.dragger
                        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
                        |> Maybe.withDefault model.drag
              }
            , Cmd.none
            )


doc : Model -> Browser.Document Msg
doc model =
    { title = "Sandb8x"
    , body = view model |> List.singleton
    }


type alias Config =
    { width : Int
    , height : Int
    }


viewport =
    { width = 800
    , height = 600
    }


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width viewport.width
        , height viewport.height
        ]
        (scene (getDrag model) model.theta)


scene : Vec2 -> Float -> List Entity
scene drag theta =
    [ WebGL.entity
        vertexShader
        fragmentShader
        (DDD.Mesh.Cube.colorful 1 1 0.1)
        (uniforms drag theta)
    , WebGL.entity
        vertexShader
        fragmentShader
        (DDD.Mesh.Cube.colorful 0.2 0.2 0.2)
        (boxUniforms theta)
    ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , directionalLight : Vec3
    , pointLight : Vec3
    }


directionalLight =
    Vec3.fromRecord { x = 0, y = -1, z = 3 }


camera =
    Mat4.makeLookAt
        (Vec3.fromRecord { x = 0, y = -4, z = 2 })
        (vec3 0 0 0)
        (vec3 0 1 0)


uniforms : Vec2 -> Float -> Uniforms
uniforms drag theta =
    { rotation =
        Mat4.identity
            |> Mat4.rotate (Vec2.getY drag * 0.01) (vec3 1 0 0)
            |> Mat4.rotate (Vec2.getX drag * 0.01) (vec3 0 0 1)
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = camera
    , directionalLight = directionalLight
    , pointLight = vec3 0 -5 5
    }


boxUniforms : Float -> Uniforms
boxUniforms theta =
    { rotation = Mat4.makeRotate (6 * theta) (vec3 0 0 1)

    --            Mat4.makeRotate (Vec2.getX rotate) (vec3 0 1 0)
    --                |> Mat4.rotate (Vec2.getY rotate) (vec3 1 0 0)
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = camera
    , directionalLight = directionalLight
    , pointLight = vec3 0 -5 5
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_position : Vec3
    , v_lighting : Vec3
    }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform vec3 directionalLight;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying highp vec3 v_lighting;


        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);

            highp vec3 ambientLight = vec3(0, 0, 0);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = rotation * vec4(normal, 1.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            v_lighting = ambientLight + (directionalLightColor * directional);
            v_color = color;
            v_normal = normal;
            v_position = position;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;


        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform vec3 directionalLight;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying vec3 v_lighting;

        void main () {
            gl_FragColor = vec4(v_color * v_lighting, 1.0);
        }
    |]
