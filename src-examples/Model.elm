module Model exposing
    ( ActiveScene(..)
    , DragTarget(..)
    , Hud(..)
    , HudMsg(..)
    , HudObject(..)
    , HudValue(..)
    , Model
    , Msg(..)
    , currentSceneName
    , dragTarget
    , getDrag
    , init
    , loadScene
    , mapSceneOptions
    , nextScene
    , prevScene
    , sceneOptions
    , updateAssetStore
    , viewport
    )

import Array exposing (Array)
import Asset
import Browser.Dom
import Keyboard
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3
import Scenes.Animals
import Scenes.BrickWall
import Scenes.Landscape
import Scenes.Light
import Scenes.NormalMapping
import Scenes.Sandbox
import Scenes.Textures
import XYZMika.Debug as Dbug
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Options as SceneOptions


type Msg
    = Animate Float
    | OnViewportElement (Result Browser.Dom.Error Browser.Dom.Element)
    | OnResize
    | DragStart DragTarget Vec2
    | Drag Vec2
    | DragBy Vec2
    | DragEnd Vec2
    | AssetLoaded Float AssetStore.Content
    | AssetLoadedWithTransform Mat4 Float AssetStore.Content
    | AssetLoadedToDownload String Asset.Obj Float AssetStore.Content
    | AssetLoadedToDownloadWithTransform String Asset.Obj Mat4 Float AssetStore.Content
      --
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
      --
    | HudMsg HudMsg
    | SetValue HudObject HudValue String


type DragTarget
    = Default
    | CameraOrbit
    | CameraPan
    | CameraZoom


viewport =
    { width = 1600
    , height = 800
    }


dragTarget : Model -> DragTarget
dragTarget model =
    [ ( Keyboard.isKeyDown Keyboard.Alt model.keyboard && Keyboard.isKeyDown Keyboard.Shift model.keyboard
      , CameraPan
      )
    , ( Keyboard.isKeyDown Keyboard.Alt model.keyboard
      , CameraOrbit
      )
    , ( Keyboard.isKeyDown Keyboard.Shift model.keyboard
      , CameraZoom
      )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault Default


type HudMsg
    = Click
    | ToggleSidebar


type alias Model =
    { theta : Float
    , paused : Bool
    , viewPortElement : Maybe Browser.Dom.Element
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    , lastDrag : Vec2
    , dragTarget : DragTarget
    , scene : Maybe (Scene Material.Name)
    , sceneOptions : SceneOptions.Options
    , scenes : Array ActiveScene
    , currentSceneIndex : Int
    , assets : AssetStore.Store Asset.Obj Asset.Texture
    , hud : Hud
    , keyboard : Keyboard.State
    , selectedTreeIndex : Maybe Int
    }


type Hud
    = Hud { sidebarExpanded : Bool }


type HudValue
    = HudValue_Vec3_X
    | HudValue_Vec3_Y
    | HudValue_Vec3_Z
    | HudValue_Vec3_Roll


type HudObject
    = Camera
    | SelectedGraph


getDrag model =
    model.dragger
        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
        |> Maybe.withDefault model.drag


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , paused = False
    , dragger = Nothing
    , drag = vec2 0 0
    , lastDrag = vec2 0 0
    , dragTarget = Default
    , scene = Nothing
    , sceneOptions = SceneOptions.create
    , scenes = [ BrickWall, Animals, Textures, NormalMapping, Light, Sandbox, Landscape ] |> Array.fromList
    , currentSceneIndex = 3
    , assets = AssetStore.init Asset.objPath Asset.texturePath
    , hud = Hud { sidebarExpanded = True }
    , keyboard = Keyboard.init
    , viewPortElement = Nothing
    , selectedTreeIndex = Nothing
    }
        |> loadScene
        |> (\( model, cmd ) ->
                ( model
                , Cmd.batch
                    [ cmd
                    , AssetStore.loadTexture Asset.Empty model.assets (AssetLoaded 0.1)
                    , AssetStore.loadTexture Asset.Placeholder model.assets (AssetLoaded 0.1)
                    ]
                )
           )


mapSceneOptions : (SceneOptions.Options -> SceneOptions.Options) -> Model -> Model
mapSceneOptions f model =
    { model | sceneOptions = f model.sceneOptions }


nextScene : Model -> ( Model, Cmd Msg )
nextScene model =
    { model
        | currentSceneIndex = model.currentSceneIndex + 1 |> modBy (Array.length model.scenes)
        , sceneOptions = SceneOptions.create
    }
        |> loadScene


prevScene : Model -> ( Model, Cmd Msg )
prevScene model =
    { model
        | currentSceneIndex = model.currentSceneIndex - 1 |> modBy (Array.length model.scenes)
        , sceneOptions = SceneOptions.create
    }
        |> loadScene



-- PRIVATE


type ActiveScene
    = BrickWall
    | NormalMapping
    | Textures
    | Sandbox
    | Animals
    | Light
    | Landscape


currentSceneName : Model -> String
currentSceneName model =
    case Array.get model.currentSceneIndex model.scenes of
        Just BrickWall ->
            "BrickWall"

        Just NormalMapping ->
            "NormalMapping"

        Just Textures ->
            "Textures"

        Just Sandbox ->
            "Sandbox"

        Just Animals ->
            "Animals"

        Just Landscape ->
            "Landscape"

        Just Light ->
            "Light"

        Nothing ->
            "-"


sceneOptions : Model -> Maybe Scene.Options
sceneOptions model =
    case Array.get model.currentSceneIndex model.scenes of
        Just BrickWall ->
            Scenes.BrickWall.sceneOptions

        Just NormalMapping ->
            Scenes.NormalMapping.sceneOptions

        Just Textures ->
            Scenes.Textures.sceneOptions

        Just Sandbox ->
            Scenes.Sandbox.sceneOptions

        Just Animals ->
            Scenes.Animals.sceneOptions

        Just Landscape ->
            Scenes.Landscape.sceneOptions

        Just Light ->
            Scenes.Light.sceneOptions

        Nothing ->
            Nothing


updateAssetStore : AssetStore.Store Asset.Obj Asset.Texture -> Model -> Model
updateAssetStore assets model =
    { model | assets = assets }
        |> (\m ->
                case Array.get m.currentSceneIndex m.scenes of
                    Just BrickWall ->
                        { m | scene = Scenes.BrickWall.init m.assets |> Just }

                    Just NormalMapping ->
                        { m | scene = Scenes.NormalMapping.init m.assets |> Just }

                    Just Textures ->
                        { m | scene = Scenes.Textures.init m.assets |> Just }

                    Just Sandbox ->
                        m

                    Just Animals ->
                        { m | scene = Scenes.Animals.init m.assets |> Just }

                    Just Landscape ->
                        m

                    Just Light ->
                        m

                    Nothing ->
                        m
           )


loadScene : Model -> ( Model, Cmd Msg )
loadScene model =
    case Array.get model.currentSceneIndex model.scenes |> Dbug.log "loadScene" of
        Just NormalMapping ->
            { model | scene = Just <| Scenes.NormalMapping.init model.assets }
                |> (\model_ ->
                        ( model_ |> mapSceneOptions (SceneOptions.toggle SceneOptions.showGridYOption)
                        , Cmd.batch
                            [ AssetStore.loadXyz Asset.SneakerXyz
                                model_.assets
                                (AssetLoaded 1.0)

                            --, AssetStore.loadObj Asset.Sneaker
                            --    model_.assets
                            --    (AssetLoadedToDownloadWithTransform "sneaker"
                            --        Asset.Sneaker
                            --        (Mat4.makeRotate (-0.5 * pi) Vec3.i)
                            --        0.3
                            --    )
                            , AssetStore.loadTexture Asset.SneakerDiffuse model_.assets (AssetLoaded 1)
                            , AssetStore.loadTexture Asset.SneakerNormal model_.assets (AssetLoaded 1)
                            ]
                        )
                   )

        Just BrickWall ->
            { model | scene = Just <| Scenes.BrickWall.init model.assets }
                |> (\model_ ->
                        ( model_
                        , Cmd.batch
                            [ AssetStore.loadObj Asset.Cube model_.assets (AssetLoaded 1)
                            , AssetStore.loadTexture Asset.BrickWallDiffuse model_.assets (AssetLoaded 1)
                            , AssetStore.loadTexture Asset.BrickWallNormal model_.assets (AssetLoaded 1)
                            ]
                        )
                   )

        Just Textures ->
            { model | scene = Just <| Scenes.Textures.init model.assets }
                |> (\model_ ->
                        ( model_
                        , Cmd.batch
                            [ AssetStore.loadObj Asset.Ball model_.assets (AssetLoaded 0.1)
                            , AssetStore.loadTexture Asset.BallDiffuse model_.assets (AssetLoaded 0.1)
                            , AssetStore.loadTexture Asset.BallNormal model_.assets (AssetLoaded 0.1)
                            , AssetStore.loadObj Asset.Tree model_.assets (AssetLoaded 0.1)
                            , AssetStore.loadTexture Asset.TreeDiffuse model_.assets (AssetLoaded 0.1)
                            ]
                        )
                   )

        Just Sandbox ->
            ( { model | scene = Just Scenes.Sandbox.init }
            , Cmd.none
            )

        Just Animals ->
            { model | scene = Just <| Scenes.Animals.init model.assets }
                |> (\model_ ->
                        ( model_
                        , Cmd.batch
                            [ AssetStore.loadObj Asset.Deer model_.assets (AssetLoaded 0.001)
                            , AssetStore.loadObj Asset.Wolf model_.assets (AssetLoaded 0.001)
                            , AssetStore.loadObj Asset.Cat model_.assets (AssetLoaded 0.001)

                            --, AssetStore.loadObj Asset.Monkey model_.assets (AssetLoaded 0.1)
                            ]
                        )
                   )

        Just Landscape ->
            ( { model
                | scene = Just Scenes.Landscape.init
              }
            , Cmd.none
            )

        Just Light ->
            ( { model
                | scene = Just Scenes.Light.init
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )
