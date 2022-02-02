port module Sandbox exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Json.Decode as D
import Keyboard
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.GridWorld as GridWorld exposing (GridWorld)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Generator.Perlin as Perlin
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Landscape


port onPointerMove : ({ x : Int, y : Int } -> msg) -> Sub msg


playerHeight =
    1


landscapeHeight =
    3


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
    vec3 0.3 0.6 1
        |> Vec3.scale water
        |> Vec3.add (vec3 0.3 0.95 0.35 |> Vec3.scale grass)
        |> Vec3.add (vec3 0.847 0.839 0.811 |> Vec3.scale cliffs)
        |> Vec3.add (vec3 1 1 1 |> Vec3.scale snow)


elevation x y =
    let
        base =
            Perlin.value2d { seed = 42, freq = 0.01 } x y
                + Perlin.value2d { seed = 242, freq = 0.005 } x y
    in
    base
        + (0.5
            * Perlin.value2d { seed = 12, freq = 0.05 } x y
            * Perlin.value2d { seed = 7, freq = 0.09 } x y
          )
        + (0.2 * Perlin.value2d { seed = 2, freq = 0.03 } x y)
        + (max base 0 * Perlin.value2d { seed = 19, freq = 0.05 } x y)


type alias Model =
    { theta : Float
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    , keyboard : Keyboard.State
    , camera : Camera
    , player : Player
    , terrain : Dict ( Int, Int ) (Mesh Vertex)
    , gridWorld : GridWorld Vertex
    , assets : AssetStore.Store ObjId TextureId
    }


type ObjId
    = Ball
    | Tree


objPath : ObjId -> String
objPath texture =
    case texture of
        Ball ->
            "obj/Basketball_size6_SF/Basketball_size6_SF.obj"

        Tree ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.obj"


type TextureId
    = BallDiffuse
    | BrickWall
    | TreeDiffuse


texturePath : TextureId -> String
texturePath texture =
    case texture of
        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"

        BrickWall ->
            "img/brickwall-1.jpg"

        TreeDiffuse ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.png"


getDrag model =
    model.dragger
        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
        |> Maybe.withDefault model.drag


type PlayerVerb
    = Idle
    | Walking
    | Running


type alias Player =
    { position : Vec2
    , direction : Vec2
    , movement : Vec2
    , verb : PlayerVerb
    , mesh : Mesh Vertex
    }


type alias Camera =
    { position : Vec3
    , focus : Vec3
    , zoom : Float
    , fov : Float
    }


startWithChunksOrIsIt =
    1


fovMin =
    60


fovMax =
    140


initModel : Model
initModel =
    { theta = 0
    , dragger = Nothing
    , drag = vec2 0 0
    , keyboard = Keyboard.init
    , camera = Camera (vec3 0 0 0) (vec3 0 0 0) 1 fovMax
    , player =
        { position = vec2 0 0
        , direction = vec2 0 1
        , movement = vec2 0 0
        , verb = Idle
        , mesh =
            XYZMika.XYZ.Mesh.Cube.gray (playerHeight / 1) playerHeight (playerHeight / 1)
                |> WebGL.triangles
        }
    , terrain = Dict.empty
    , gridWorld =
        GridWorld.init (GridWorld.withGenerator generator)
            |> GridWorld.generateChunks ( -startWithChunksOrIsIt, -startWithChunksOrIsIt ) ( startWithChunksOrIsIt, startWithChunksOrIsIt )
    , assets = AssetStore.init objPath texturePath
    }


generator : ( Int, Int ) -> ( Vec2, Vec2 ) -> Mesh Vertex
generator ( ix, iy ) ( p1, p2 ) =
    let
        ( width, length ) =
            ( Vec2.getX p2 - Vec2.getX p1
            , Vec2.getY p2 - Vec2.getY p1
            )

        options : XYZMika.XYZ.Mesh.Landscape.Options
        options =
            { divisions = 8
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
    XYZMika.XYZ.Mesh.Landscape.simple options
        |> (\( v, vmap ) -> WebGL.indexedTriangles v vmap)


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
    | OnPointerMove { x : Int, y : Int }
    | AssetLoaded (Result AssetStore.Error AssetStore.Content)


main : Program () Model Msg
main =
    Browser.document
        { init =
            always
                ( initModel
                , Cmd.batch
                    [ AssetStore.loadObjWithScale 0.1 Ball initModel.assets AssetLoaded
                    , AssetStore.loadTexture BallDiffuse initModel.assets AssetLoaded
                    , AssetStore.loadTexture BrickWall initModel.assets AssetLoaded
                    , AssetStore.loadObjWithScale 0.1 Tree initModel.assets AssetLoaded
                    , AssetStore.loadTexture TreeDiffuse initModel.assets AssetLoaded
                    ]
                )
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
        playerY : Float
        playerY =
            elevation
                (model.player.position |> Vec2.getX)
                (model.player.position |> Vec2.getY)
                * landscapeHeight
                + playerHeight

        ( above, behind ) =
            ( 1, 2 )

        position : Vec3
        position =
            model.player.direction
                |> Vec2.normalize
                |> Vec2.scale (4 * camera_.zoom + behind)
                |> Vec2.sub model.player.position
                |> Vec2.toRecord
                |> (\{ x, y } -> vec3 x (playerY + above + 2 * (camera_.zoom ^ 3)) y)

        focus : Vec3
        focus =
            model.player.direction
                --|> Vec2.normalize
                --|> Vec2.scale (20 * (camera_.zoom - 1))
                --|> Vec2.negate
                |> Vec2.add model.player.position
                |> Vec2.toRecord
                |> (\{ x, y } -> vec3 x (playerY - 1) y)

        --fov : Vec3
        --fov =
        --    model.player.direction
        --        |> Vec2.normalize
        --        |> Vec2.scale (20 * (camera_.zoom - 1))
        --        |> Vec2.negate
        --        |> Vec2.add model.player.position
        --        |> Vec2.toRecord
        --        |> (\{ x, y } -> vec3 x (playerY - 1) y)
        --        aboveGround p =
        --            let
        --                { x, y, z } =
        --                    Vec3.toRecord p
        --
        --                yAboveGround =
        --                    max (elevation x z + 3) y
        --            in
        --            p |> Vec3.setY yAboveGround
    in
    { model
        | camera =
            { camera_
                | focus = focus
                , position = position

                --, fov = fov
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

        zoom_ : Float
        zoom_ =
            model.camera.zoom
                |> (\y -> y + dy)
                |> (\y -> max 0 y)
                |> (\y -> min 2 y)

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
        , camera =
            { camera_
                | zoom = zoom_
                , fov = fovMax - (model.camera.zoom / 2 * fovMin)
            }
    }


movePlayer : Model -> Model
movePlayer model =
    let
        direction_ =
            model.player.direction

        movementForward : Maybe Vec2
        movementForward =
            if
                Keyboard.isKeyDown Keyboard.ArrowUp model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'W') model.keyboard
            then
                direction_
                    |> Vec2.normalize
                    |> Just

            else if
                Keyboard.isKeyDown Keyboard.ArrowDown model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'S') model.keyboard
            then
                direction_
                    |> Vec2.negate
                    |> Vec2.normalize
                    |> Just

            else
                Nothing

        movementSide : Maybe Vec2
        movementSide =
            if
                Keyboard.isKeyDown Keyboard.ArrowLeft model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'A') model.keyboard
            then
                direction_
                    |> (\v -> vec2 (Vec2.getY v) -(Vec2.getX v))
                    |> Vec2.normalize
                    |> Just

            else if
                Keyboard.isKeyDown Keyboard.ArrowRight model.keyboard
                    || Keyboard.isKeyDown (Keyboard.Alpha 'D') model.keyboard
            then
                direction_
                    |> (\v -> vec2 -(Vec2.getY v) (Vec2.getX v))
                    |> Vec2.normalize
                    |> Just

            else
                Nothing

        movement : Vec2
        movement =
            Vec2.add
                (Maybe.withDefault (vec2 0 0) movementForward)
                (Maybe.withDefault (vec2 0 0) movementSide)

        player_ : Player
        player_ =
            model.player

        ( verb, speed ) =
            case
                ( Keyboard.isKeyDown Keyboard.Shift model.keyboard
                , movementForward
                , movementSide
                )
            of
                ( _, Nothing, Nothing ) ->
                    ( Idle, 0 )

                ( False, _, _ ) ->
                    ( Walking, 0.3 )

                ( True, _, _ ) ->
                    ( Running, 1.2 )
    in
    { model
        | player =
            { player_
                | position = Vec2.add model.player.position (movement |> Vec2.scale speed)
                , movement = movement
                , verb = verb
                , direction = direction_
            }
    }



-- TERRAIN


generateTerrain : Model -> Model
generateTerrain model =
    { model
        | gridWorld =
            GridWorld.generate
                (GridWorld.gridFromCoord (model.camera.focus |> (\v -> vec2 (Vec3.getX v) (Vec3.getZ v))))
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

        AssetLoaded (Ok asset) ->
            ( { model | assets = model.assets |> AssetStore.addToStore asset }, Cmd.none )

        AssetLoaded (Err error) ->
            let
                _ =
                    Debug.log "AssetLoaded" error
            in
            ( model, Cmd.none )

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
    Maybe.map5
        SceneConfig
        --(\ballMesh ->
        --    \ballTexture ->
        --        \brickWallTexture ->
        --            WebGL.toHtml
        --                [ width viewport.width
        --                , height viewport.height
        --                ]
        --                (scene (getDrag model) model)
        --)
        (AssetStore.mesh Ball model.assets)
        (AssetStore.texture BallDiffuse model.assets)
        (AssetStore.texture BrickWall model.assets)
        (AssetStore.mesh Tree model.assets)
        (AssetStore.texture TreeDiffuse model.assets)
        |> Maybe.map
            (\config ->
                WebGL.toHtml
                    [ width viewport.width
                    , height viewport.height
                    ]
                    (scene config (getDrag model) model)
            )
        |> Maybe.withDefault (Html.p [] [ Html.text "LOADING ASSETS..." ])



--(scene (getDrag model) model)


type alias SceneConfig =
    { ballMesh : Mesh Vertex
    , ballTexture : Texture
    , brickWallTexture : Texture
    , treeMesh : Mesh Vertex
    , treeTexture : Texture
    }


scene : SceneConfig -> Vec2 -> Model -> List Entity
scene config drag model =
    let
        ( px, py ) =
            ( Vec2.getX model.player.position, Vec2.getY model.player.position )

        pz x y =
            landscapeHeight * elevation x y + (playerHeight / 2)

        playerPos =
            vec3 px (pz px py) py
    in
    WebGL.entity
        vertexShader
        fragmentShader
        model.player.mesh
        (playerUniforms
            model.camera
            config.brickWallTexture
            model.player.direction
            playerPos
        )
        :: WebGL.entity
            vertexShader
            fragmentShader
            config.ballMesh
            (playerUniforms
                model.camera
                config.ballTexture
                model.player.direction
                (playerPos |> Vec3.add (vec3 0 0.5 0))
            )
        :: WebGL.entity
            vertexShaderTerrain
            fragmentShaderTerrain
            config.treeMesh
            (terrainChunkUniforms
                model.camera
                config.treeTexture
                playerPos
                ( 1, 3 )
            )
        :: (model.gridWorld
                |> GridWorld.geometry
                |> List.map
                    (\( ( x, y ), mesh ) ->
                        WebGL.entity
                            vertexShaderTerrain
                            fragmentShaderTerrain
                            mesh
                            (terrainChunkUniforms model.camera config.brickWallTexture playerPos ( x, y ))
                    )
           )


terrainChunkUniforms : Camera -> Texture -> Vec3 -> ( Float, Float ) -> Uniforms
terrainChunkUniforms camera_ texture playerPos ( x, y ) =
    { rotation = Mat4.identity
    , translate = Mat4.makeTranslate (vec3 x 0 y)
    , perspective = perspective camera_.fov
    , camera = camera camera_
    , directionalLight = directionalLight
    , playerPos = playerPos
    , cameraFocus = camera_.focus
    , receiveShadow = 1.0
    , texture = texture
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
    , texture : Texture
    }


directionalLight =
    Vec3.fromRecord { x = 1, y = 0.5, z = 1 }


camera : Camera -> Mat4
camera camera_ =
    Mat4.makeLookAt camera_.position camera_.focus (vec3 0 1 0)


aspect =
    toFloat viewport.width / toFloat viewport.height


perspective fov =
    Mat4.makePerspective fov aspect 0.01 300


playerUniforms : Camera -> Texture -> Vec2 -> Vec3 -> Uniforms
playerUniforms camera_ texture direction playerPos =
    { rotation = Mat4.makeRotate (atan2 (Vec2.getX direction) (Vec2.getY direction)) (vec3 0 1 0)
    , translate = Mat4.makeTranslate playerPos
    , perspective = perspective camera_.fov
    , camera = camera camera_
    , directionalLight = directionalLight
    , playerPos = playerPos
    , cameraFocus = camera_.focus
    , receiveShadow = 0.0
    , texture = texture
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_position : Vec3
    , v_lighting : Vec3
    , v_coord : Vec2
    }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;
        attribute vec2 uv;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 translate;
        uniform mat4 rotation;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying vec3 v_lighting;
        varying vec2 v_coord;



        void main () {
            gl_Position = perspective * camera * translate * rotation * vec4(position, 1.0);
            v_color = color;
            v_coord = uv;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        
        uniform sampler2D texture;


        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_position;
        varying vec3 v_lighting;
        varying vec2 v_coord;


        void main () {
        
            vec4 tex = texture2D(texture, v_coord) * 2.5;

            gl_FragColor = tex * vec4(v_color , 1.0);
        }
    |]



-- TERRAIN


vertexShaderTerrain : Shader Vertex Uniforms Varyings
vertexShaderTerrain =
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
        varying vec2 v_coord;

        void main () {
            vec3 wPosition = (translate * rotation * vec4(position, 1.0)).xyz;
            
            float d = length(wPosition.xz - playerPos.xz);
            vec3 yPlanetP = vec3(0.0, -(d * d * d * d * 0.00000005), 0.0);
            
            
            gl_Position = perspective * camera * vec4(wPosition + yPlanetP, 1.0);
            
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


fragmentShaderTerrain : Shader {} Uniforms Varyings
fragmentShaderTerrain =
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
        varying vec2 v_coord;

        void main () {
        
            // Player drop shadow
            float s1 = 0.0;
            float s2 = 0.9;
            float d = length(v_position.xz - playerPos.xz);
            float dClamped = min(s2, d / s2);
            float playerShadow = (dClamped + s1) / (1.0 + s1);
            float shadow = ((receiveShadow * playerShadow) + 1.0) / 2.0;
            
//             View area
            float vDistance = 80.0;
            float vDiff = length(v_position.xz - cameraFocus.xz);
            float vShadow = abs(receiveShadow * min(vDistance, vDiff) / vDistance - 1.0);
//            float vShadow = 1.0;
            
            vec4 bColor = receiveShadow * vec4(0.08627451, 0.08627451, 0.11372549, 1.0);
            vec3 lighting = receiveShadow > 0.0 ? v_lighting : vec3(1,1,1);
            gl_FragColor = bColor + vec4(3.0 * v_color * lighting * shadow * (vShadow), 1.0);
            
        }
    |]
