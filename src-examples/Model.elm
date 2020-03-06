module Model exposing
    ( Model
    , Msg(..)
    , getDrag
    , init
    , loadScene
    , mapRenderOptions
    , mapRendererOptions
    , nextScene
    , prevScene
    , sceneOptions
    , updateAssetStore
    )

import Array exposing (Array)
import Asset
import Material
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Scenes.Landscape
import Scenes.Light
import Scenes.NormalMapping
import Scenes.ObjectLoader
import Scenes.Sandbox
import Scenes.Textures
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Material
import XYZMika.XYZ.Scene as Scene exposing (Scene)


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyPressed String
    | GotObj (Maybe Material.Name) ( { scale : Float, color : Vec3 }, Vec3, String )
    | AssetLoaded Float AssetStore.Content


type alias Model =
    { theta : Float
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    , scene : Scene Material.Name
    , rendererOptions : XYZMika.XYZ.Material.Options
    , renderOptions : Scene.RenderOptions
    , scenes : Array ActiveScene
    , currentSceneIndex : Int
    , assets : AssetStore.Store Asset.Obj Asset.Texture
    }


getDrag model =
    model.dragger
        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
        |> Maybe.withDefault model.drag


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , dragger = Nothing
    , drag = vec2 0 0
    , scene = Scenes.Light.init
    , rendererOptions = XYZMika.XYZ.Material.defaultOptions
    , renderOptions = Scene.RenderOptions True False False
    , scenes = [ NormalMapping, Textures, Sandbox, ObjectLoader, Light, Landscape ] |> Array.fromList
    , currentSceneIndex = 0
    , assets = AssetStore.init Asset.objPath Asset.texturePath
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


mapRenderOptions : (Scene.RenderOptions -> Scene.RenderOptions) -> Model -> Model
mapRenderOptions f model =
    { model | renderOptions = f model.renderOptions }


mapRendererOptions : (XYZMika.XYZ.Material.Options -> XYZMika.XYZ.Material.Options) -> Model -> Model
mapRendererOptions f model =
    { model | rendererOptions = f model.rendererOptions }


nextScene : Model -> ( Model, Cmd Msg )
nextScene model =
    { model
        | currentSceneIndex = model.currentSceneIndex + 1 |> modBy (Array.length model.scenes)
    }
        |> loadScene


prevScene : Model -> ( Model, Cmd Msg )
prevScene model =
    { model
        | currentSceneIndex = model.currentSceneIndex - 1 |> modBy (Array.length model.scenes)
    }
        |> loadScene



-- PRIVATE


type ActiveScene
    = NormalMapping
    | Textures
    | Sandbox
    | ObjectLoader
    | Light
    | Landscape


sceneOptions : Model -> Maybe Scene.Options
sceneOptions model =
    case Array.get model.currentSceneIndex model.scenes of
        Just NormalMapping ->
            Scenes.NormalMapping.sceneOptions

        Just Textures ->
            Scenes.Textures.sceneOptions

        Just Sandbox ->
            Scenes.Sandbox.sceneOptions

        Just ObjectLoader ->
            Scenes.ObjectLoader.sceneOptions

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
                    Just NormalMapping ->
                        { m | scene = Scenes.NormalMapping.init m.assets }

                    Just Textures ->
                        { m | scene = Scenes.Textures.init m.assets }

                    Just Sandbox ->
                        m

                    Just ObjectLoader ->
                        m

                    Just Landscape ->
                        m

                    Just Light ->
                        m

                    Nothing ->
                        m
           )


loadScene : Model -> ( Model, Cmd Msg )
loadScene model =
    case Array.get model.currentSceneIndex model.scenes of
        Just NormalMapping ->
            { model | scene = Scenes.NormalMapping.init model.assets }
                |> (\model_ ->
                        { model_
                            | rendererOptions =
                                model_.rendererOptions
                                    |> XYZMika.XYZ.Material.setDirectionalLight Scene.direction.forward
                        }
                   )
                |> (\model_ ->
                        ( model_
                        , Cmd.batch
                            [ AssetStore.loadObj Asset.Cube model_.assets (AssetLoaded 1)
                            , AssetStore.loadTexture Asset.CubeDiffuse model_.assets (AssetLoaded 1)
                            ]
                        )
                   )

        Just Textures ->
            { model | scene = Scenes.Textures.init model.assets }
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
            ( { model | scene = Scenes.Sandbox.init }
            , Cmd.none
            )

        Just ObjectLoader ->
            ( { model
                | scene = Scenes.ObjectLoader.init
              }
            , Cmd.batch
                [ Scenes.ObjectLoader.getObj
                    { scale = 0.001, color = vec3 1 0.5 0.5 }
                    (vec3 0 0 -0.5)
                    "obj/deer.obj"
                    (GotObj (Just Material.Advanced))
                , Scenes.ObjectLoader.getObj
                    { scale = 0.3, color = vec3 0.5 0.5 1 }
                    (vec3 -1 1 0)
                    "obj/monkey.obj"
                    (GotObj Nothing)
                , Scenes.ObjectLoader.getObj
                    { scale = 0.001, color = vec3 1 1 0.5 }
                    (vec3 0 0 0.5)
                    "obj/cat.obj"
                    (GotObj (Just Material.Advanced))
                , Scenes.ObjectLoader.getObj
                    { scale = 0.001, color = vec3 0.5 1 1 }
                    (vec3 0 0 0)
                    "obj/wolf.obj"
                    (GotObj Nothing)
                , Scenes.ObjectLoader.getObj
                    { scale = 1, color = vec3 0.5 1 0.5 }
                    (vec3 0 0.5 -1.5)
                    "obj/cube.obj"
                    (GotObj Nothing)
                ]
            )

        Just Landscape ->
            ( { model
                | scene = Scenes.Landscape.init
              }
            , Cmd.none
            )

        Just Light ->
            ( { model
                | scene = Scenes.Light.init
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )
