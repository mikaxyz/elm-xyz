port module Sandbox exposing (main)

import Browser
import Browser.Events
import DDD.Data.Vertex exposing (Vertex)
import DDD.Generator.Perlin as Perlin
import DDD.Mesh.Cube
import DDD.Mesh.Landscape
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Json.Decode as D
import Keyboard
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sandbox.GridWorld as GridWorld exposing (GridWorld)
import WebGL exposing (Entity, Mesh, Shader)


port onPointerMove : ({ x : Int, y : Int } -> msg) -> Sub msg


playerHeight =
    1


landscapeHeight =
    2


color height_ =
    let
        normalize min max val =
            (val - min) / (max - min)

        h =
            normalize -1 1 height_

        ht =
            abs (h - 1)

        between y1 y2 =
            ((ht - (1 - y1)) / (y2 - y1)) * ((h - y2) / (y2 - y1))

        water =
            (ht - 0.7)
                / 0.3
                |> clamp 0 1

        grass =
            between 0.1 0.8
                |> clamp 0 1

        cliffs =
            between 0.7 0.8
                |> clamp 0 1

        snow =
            (h - 0.7)
                / 0.3
                |> clamp 0 1
    in
    vec3 0 0.1 1
        |> Vec3.scale water
        |> Vec3.add (vec3 0 1 0 |> Vec3.scale grass)
        |> Vec3.add (vec3 0.847 0.839 0.811 |> Vec3.scale cliffs)
        |> Vec3.add (vec3 1 1 1 |> Vec3.scale snow)


elevation x y =
    let
        seed =
            42

        freq =
            0.02

        e1 =
            Perlin.value2d { seed = seed, freq = freq } x y
    in
    e1
        + Perlin.value2d { seed = seed, freq = 1.0 * freq } x y
        + (Perlin.value2d { seed = seed, freq = 5.0 * freq } x y
            * Perlin.value2d { seed = seed, freq = 8.0 * freq } x y
          )
        + (e1 * max e1 0 * Perlin.value2d { seed = seed, freq = 20 * freq } x y)


type alias Model =
    { theta : Float
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    , keyboard : Keyboard.State
    , camera : Camera
    , player :
        { position : Vec2
        , direction : Vec2
        , movement : Vec2
        , mesh : Mesh Vertex
        }
    , terrain : Dict ( Int, Int ) (Mesh Vertex)
    , gridWorld : GridWorld Vertex
    }


getDrag model =
    model.dragger
        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
        |> Maybe.withDefault model.drag


type alias Camera =
    { position : Vec3
    , focus : Vec3
    , zoom : Float
    }


initModel : Model
initModel =
    { theta = 0
    , dragger = Nothing
    , drag = vec2 0 0
    , keyboard = Keyboard.init
    , camera = Camera (vec3 0 0 0) (vec3 0 0 0) 1.0
    , player =
        { position = vec2 0 0
        , direction = vec2 0 1
        , movement = vec2 0 0
        , mesh = DDD.Mesh.Cube.gray (playerHeight / 8) playerHeight (playerHeight / 8)
        }
    , terrain = Dict.empty
    , gridWorld =
        GridWorld.init (GridWorld.withGenerator generator)
            |> GridWorld.generateChunks ( -2, -2 ) ( 2, 2 )
    }


generator : ( Int, Int ) -> ( Vec2, Vec2 ) -> Mesh Vertex
generator ( ix, iy ) ( p1, p2 ) =
    let
        ( width, length ) =
            ( Vec2.getX p2 - Vec2.getX p1
            , Vec2.getY p2 - Vec2.getY p1
            )

        options : DDD.Mesh.Landscape.Options
        options =
            { divisions = 23
            , width = width / 2
            , length = length / 2
            , height = landscapeHeight
            , color = color
            , elevation =
                \x y ->
                    elevation
                        (Vec2.getX p1 + x + (width / 2))
                        (Vec2.getY p1 + y + (length / 2))

            --                        + (toFloat (iy + ix) * 0.1)
            }
    in
    DDD.Mesh.Landscape.simple options
        |> (\( v, vmap ) -> WebGL.indexedTriangles v vmap)


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
    | OnPointerMove { x : Int, y : Int }


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
        --        [ drags
        [ Browser.Events.onAnimationFrameDelta Animate
        , Keyboard.subscriptions { tagger = KeyboardMsg, keyDown = OnKeyDown }
        , onPointerMove OnPointerMove
        ]


moveCamera : Camera -> Model -> Model
moveCamera camera_ model =
    let
        position =
            model.player.direction
                |> Vec2.normalize
                |> Vec2.scale (8 * camera_.zoom + 8)
                |> Vec2.sub model.player.position
                |> Vec2.toRecord
                |> (\{ x, y } -> vec3 x (20 * camera_.zoom * camera_.zoom + 4) y)

        focus =
            model.player.direction
                |> Vec2.normalize
                |> Vec2.scale (12 * (camera_.zoom - 1))
                |> Vec2.negate
                |> Vec2.add model.player.position
                |> Vec2.toRecord
                |> (\{ x, y } -> vec3 x 0 y)
    in
    { model
        | camera =
            { camera_
                | focus = focus
                , position = position
            }
    }


handlePointerMove : Vec2 -> Model -> Model
handlePointerMove pointerMove model =
    let
        ( dx, dy ) =
            ( Vec2.getX pointerMove
            , Vec2.getY pointerMove
            )

        -- Camera
        camera_ =
            model.camera

        zoom_ =
            model.camera.zoom
                |> (\y -> y + dy)
                |> (\y -> max 0 y)
                |> (\y -> min 1 y)

        player =
            model.player

        ( cx, cy ) =
            ( Vec2.getX model.player.direction
            , Vec2.getY model.player.direction
            )

        ( px, py ) =
            ( cx * cos dx - cy * sin dx
            , cx * sin dx + cy * cos dx
            )
    in
    { model
        | player = { player | direction = vec2 px py }
        , camera = { camera_ | zoom = zoom_ }
    }


movePlayer : Model -> Model
movePlayer model =
    let
        m =
            0.3

        direction_ =
            model.player.direction

        movementForward =
            if
                Keyboard.isKeyDown Keyboard.ArrowUp model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'W') model.keyboard
            then
                direction_
                    |> Vec2.normalize
                    |> Vec2.scale m

            else if
                Keyboard.isKeyDown Keyboard.ArrowDown model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'S') model.keyboard
            then
                direction_
                    |> Vec2.negate
                    |> Vec2.normalize
                    |> Vec2.scale m

            else
                vec2 0 0

        movementSide =
            if
                Keyboard.isKeyDown Keyboard.ArrowLeft model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'A') model.keyboard
            then
                direction_
                    |> (\v -> vec2 (Vec2.getY v) -(Vec2.getX v))
                    |> Vec2.normalize
                    |> Vec2.scale m

            else if
                Keyboard.isKeyDown Keyboard.ArrowRight model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'D') model.keyboard
            then
                direction_
                    |> (\v -> vec2 -(Vec2.getY v) (Vec2.getX v))
                    |> Vec2.normalize
                    |> Vec2.scale m

            else
                vec2 0 0

        movement =
            Vec2.add movementForward movementSide

        player_ =
            model.player
    in
    { model
        | player =
            { player_
                | position = Vec2.add model.player.position movement
                , movement = movement
                , direction = direction_
            }
    }



-- TERRAIN


generateTerrain : Model -> Model
generateTerrain model =
    { model
        | gridWorld =
            GridWorld.generate
                (GridWorld.gridFromCoord model.player.position)
                model.gridWorld
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }
                |> movePlayer
                |> moveCamera model.camera
                |> generateTerrain
            , Cmd.none
            )

        OnPointerMove { x, y } ->
            ( model
                |> handlePointerMove
                    (vec2 (toFloat x / 100) (toFloat y / 100))
            , Cmd.none
            )

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

        KeyboardMsg msg_ ->
            ( { model | keyboard = Keyboard.update msg_ model.keyboard }
            , Cmd.none
            )

        OnKeyDown key ->
            let
                _ =
                    case key of
                        Keyboard.Space ->
                            Just
                                ( model.player.position
                                , model.player.position
                                    |> Vec2.toRecord
                                    |> (\{ x, y } -> elevation x y)
                                )
                                |> Debug.log "pos"

                        _ ->
                            Nothing
            in
            ( model, Cmd.none )


doc : Model -> Browser.Document Msg
doc model =
    { title = "Sandb8x"
    , body = view model |> List.singleton
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
        (scene (getDrag model) model)


scene : Vec2 -> Model -> List Entity
scene drag model =
    let
        ( px, py ) =
            ( Vec2.getX model.player.position, Vec2.getY model.player.position )

        pz x y =
            landscapeHeight * elevation x y + (playerHeight / 2)

        playerPos =
            vec3 px (pz px py) py

        uniforms =
            sceneUniforms model.camera playerPos drag
    in
    WebGL.entity
        vertexShader
        fragmentShader
        model.player.mesh
        (playerUniforms
            model.camera
            model.player.direction
            playerPos
            (Mat4.makeTranslate playerPos)
            uniforms.rotation
        )
        :: (model.gridWorld
                |> GridWorld.geometry
                |> List.map
                    (\( ( x, y ), mesh ) ->
                        WebGL.entity
                            vertexShader
                            fragmentShader
                            mesh
                            (terrainChunkUniforms model.camera playerPos ( x, y ))
                    )
           )


terrainChunkUniforms : Camera -> Vec3 -> ( Float, Float ) -> Uniforms
terrainChunkUniforms camera_ playerPos ( x, y ) =
    { rotation = Mat4.identity
    , translate = Mat4.makeTranslate (vec3 x 0 y)
    , perspective = perspective
    , camera = camera camera_
    , directionalLight = directionalLight
    , playerPos = playerPos
    , cameraFocus = camera_.focus
    , receiveShadow = 1.0
    }


type alias Uniforms =
    { rotation : Mat4
    , translate : Mat4
    , perspective : Mat4
    , camera : Mat4
    , directionalLight : Vec3
    , playerPos : Vec3
    , cameraFocus : Vec3
    , receiveShadow : Float
    }


directionalLight =
    Vec3.fromRecord { x = 1, y = 0.5, z = 1 }


camera : Camera -> Mat4
camera camera_ =
    Mat4.makeLookAt camera_.position camera_.focus (vec3 0 1 0)


aspect =
    toFloat viewport.width / toFloat viewport.height


perspective =
    Mat4.makePerspective 45 aspect 0.01 100


sceneUniforms : Camera -> Vec3 -> Vec2 -> Uniforms
sceneUniforms camera_ playerPos drag =
    { rotation = Mat4.identity
    , translate = Mat4.identity
    , perspective = perspective
    , camera = camera camera_
    , directionalLight = directionalLight
    , playerPos = playerPos
    , cameraFocus = camera_.focus
    , receiveShadow = 0.0
    }


playerUniforms : Camera -> Vec2 -> Vec3 -> Mat4 -> Mat4 -> Uniforms
playerUniforms camera_ direction playerPos position rotation =
    { rotation = Mat4.makeRotate (atan2 (Vec2.getX direction) (Vec2.getY direction)) (vec3 0 1 0)
    , translate = position
    , perspective = perspective
    , camera = camera camera_
    , directionalLight = directionalLight
    , playerPos = playerPos
    , cameraFocus = camera_.focus
    , receiveShadow = 0.0
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
        uniform mat4 translate;
        uniform vec3 directionalLight;
        uniform vec3 playerPos;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying highp vec3 v_lighting;


        void main () {
            float d = length(position.xz - playerPos.xz) * 0.5;
            gl_Position = perspective * camera * translate * rotation * vec4(position, 1.0);
            
            highp vec3 ambientLight = vec3(0.0, 0.05, 0.01);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = rotation * vec4(normalize(normal), 0.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            v_lighting = ambientLight + (directionalLightColor * directional);
            v_color = color;
            v_normal = normal;
            v_position = (translate * rotation * vec4(position, 1.0)).xyz;
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
        uniform vec3 playerPos;
        uniform vec3 cameraFocus;
        uniform float receiveShadow;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying vec3 v_lighting;

        void main () {
        
            // Player drop shadow
            float s1 = 0.0;
            float s2 = 0.9;
            float d = length(v_position.xz - playerPos.xz);
            float dClamped = min(s2, d / s2);
            float playerShadow = (dClamped + s1) / (1.0 + s1);
            float shadow = ((receiveShadow * playerShadow) + 1.0) / 2.0;
            
            // View area
            float vDistance = 16.0;
            float vDiff = length(v_position.xz - cameraFocus.xz);
            float vShadow = abs(receiveShadow * min(vDistance, vDiff) / vDistance - 1.0);
            
            vec4 bColor = receiveShadow * vec4(0.08627451, 0.08627451, 0.11372549, 1.0);
            vec3 lighting = receiveShadow > 0.0 ? v_lighting : vec3(1,1,1);
            gl_FragColor = bColor + vec4(3.0 * v_color * lighting * shadow * (vShadow), 1.0);
            
//            gl_FragColor = vec4(v_color , 1.0);
        }
    |]
