module Model exposing
    ( ActiveScene(..)
    , DragTarget(..)
    , Hud(..)
    , HudMsg(..)
    , HudObject(..)
    , HudValue(..)
    , Model
    , Msg(..)
    , SceneObject(..)
    , currentSceneName
    , dragTarget
    , init
    , loadScene
    , mapSceneOptions
    , modifiers
    , nextScene
    , prevScene
    , updateAssetStore
    , viewport
    )

import Array exposing (Array)
import Asset
import Browser.Dom
import Keyboard
import Material
import Math.Vector2 as Vec2 exposing (Vec2)
import Scenes.Animals
import Scenes.BrickWall
import Scenes.Landscape
import Scenes.Light
import Scenes.NormalMapping
import Scenes.Sandbox
import Scenes.Textures
import XYZMika.Debug as Dbug
import XYZMika.Dragon as Dragon exposing (Dragon)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Scene exposing (Scene)
import XYZMika.XYZ.Scene.Options as SceneOptions


type Msg
    = Animate Float
    | OnViewportElement (Result Browser.Dom.Error Browser.Dom.Element)
    | OnResize
    | AssetStoreLoadResult (Result AssetStore.Error AssetStore.Content)
    | AssetStoreLoadResultDownloadXyz String Asset.Obj AssetStore.Content
      --
    | OnMouseUp Vec2
      --
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
      --
    | DragonMsg Dragon.Msg
    | DragonOnDrag Dragon.Vector
      --
    | HudMsg HudMsg
    | SetValue HudObject HudValue String


type SceneObject
    = LightSceneObjectId Scenes.Light.ObjectId
    | NormalMappingSceneObjectId Scenes.NormalMapping.ObjectId
    | TexturesSceneObjectId Scenes.Textures.ObjectId


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
    , dragTarget : DragTarget
    , scene : Maybe (Scene SceneObject Material.Name)
    , sceneOptions : SceneOptions.Options
    , scenes : Array ActiveScene
    , currentSceneIndex : Int
    , assets : AssetStore.Store Asset.Obj Asset.Texture
    , hud : Hud
    , keyboard : Keyboard.State
    , dragon : Dragon
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


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , paused = False
    , dragTarget = Default
    , scene = Nothing
    , sceneOptions = SceneOptions.create
    , scenes = [ BrickWall, Animals, Textures, NormalMapping, Light, Sandbox, Landscape ] |> Array.fromList
    , currentSceneIndex = 3
    , assets = AssetStore.init Asset.objPath Asset.texturePath
    , hud = Hud { sidebarExpanded = True }
    , keyboard = Keyboard.init
    , dragon = Dragon.init
    , viewPortElement = Nothing
    , selectedTreeIndex = Nothing
    }
        |> loadScene
        |> (\( model, cmd ) ->
                ( model
                , Cmd.batch
                    [ cmd
                    , AssetStore.loadTexture Asset.MissingFile model.assets AssetStoreLoadResult
                    , AssetStore.loadTexture Asset.Placeholder model.assets AssetStoreLoadResult
                    ]
                )
           )


modifiers : Model -> List (XYZMika.XYZ.Scene.Modifier SceneObject Material.Name)
modifiers model =
    case Array.get model.currentSceneIndex model.scenes of
        Just BrickWall ->
            []

        Just NormalMapping ->
            Scenes.NormalMapping.modifiers model.theta NormalMappingSceneObjectId

        Just Textures ->
            Scenes.Textures.modifiers model.theta TexturesSceneObjectId

        Just Sandbox ->
            []

        Just Animals ->
            []

        Just Landscape ->
            []

        Just Light ->
            Scenes.Light.modifiers model.theta LightSceneObjectId

        Nothing ->
            []


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


updateAssetStore : AssetStore.Store Asset.Obj Asset.Texture -> Model -> Model
updateAssetStore assets model =
    { model | assets = assets }
        |> (\m ->
                case Array.get m.currentSceneIndex m.scenes of
                    Just BrickWall ->
                        { m | scene = Scenes.BrickWall.init m.assets |> Just }

                    Just NormalMapping ->
                        { m | scene = Scenes.NormalMapping.init NormalMappingSceneObjectId m.assets |> Just }

                    Just Textures ->
                        { m | scene = Scenes.Textures.init TexturesSceneObjectId m.assets |> Just }

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
            { model | scene = Just <| Scenes.NormalMapping.init NormalMappingSceneObjectId model.assets }
                |> (\model_ ->
                        ( model_ |> mapSceneOptions (SceneOptions.toggle SceneOptions.showGridYOption)
                        , Cmd.batch
                            [ AssetStore.loadXyz Asset.SneakerXyz model_.assets AssetStoreLoadResult

                            --, AssetStore.loadObj Asset.Sneaker
                            --    model_.assets
                            --    (AssetStoreLoadResultDownloadXyz "sneaker"
                            --        Asset.Sneaker
                            --    )
                            , AssetStore.loadTexture Asset.SneakerDiffuse model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.SneakerNormal model_.assets AssetStoreLoadResult
                            ]
                        )
                   )

        Just BrickWall ->
            { model | scene = Just <| Scenes.BrickWall.init model.assets }
                |> (\model_ ->
                        ( model_
                        , Cmd.batch
                            [ AssetStore.loadObj Asset.Cube model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.BrickWallDiffuse model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.BrickWallNormal model_.assets AssetStoreLoadResult
                            ]
                        )
                   )

        Just Textures ->
            { model | scene = Just <| Scenes.Textures.init TexturesSceneObjectId model.assets }
                |> (\model_ ->
                        ( model_ |> mapSceneOptions (SceneOptions.toggle SceneOptions.showGridYOption)
                        , Cmd.batch
                            [ AssetStore.loadObjWithScale 0.1 Asset.Ball model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.BallDiffuse model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.BallNormal model_.assets AssetStoreLoadResult
                            , AssetStore.loadObjWithScale 0.1 Asset.Tree model_.assets AssetStoreLoadResult
                            , AssetStore.loadTexture Asset.TreeDiffuse model_.assets AssetStoreLoadResult
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
                            [ AssetStore.loadObjWithScale 0.001 Asset.Deer model_.assets AssetStoreLoadResult
                            , AssetStore.loadObjWithScale 0.001 Asset.Wolf model_.assets AssetStoreLoadResult
                            , AssetStore.loadObjWithScale 0.001 Asset.Cat model_.assets AssetStoreLoadResult

                            --, AssetStore.loadObj Asset.Monkey model_.assets (AssetLoaded 0.1)
                            ]
                        )
                   )

        Just Landscape ->
            ( { model | scene = Just Scenes.Landscape.init }
            , Cmd.none
            )

        Just Light ->
            ( { model
                | scene = Just (Scenes.Light.init LightSceneObjectId)
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )
