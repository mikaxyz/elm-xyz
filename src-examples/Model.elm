module Model exposing
    ( Model
    , Msg(..)
    , getDrag
    , init
    , nextScene
    , prevScene
    , sceneOptions
    )

import Array exposing (Array)
import Asset
import Material
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Scenes.Landscape
import Scenes.Light
import Scenes.ObjectLoader
import Scenes.Sandbox
import Scenes.Textures
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
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
    , scenes = [ Textures, Sandbox, ObjectLoader, Light, Landscape ] |> Array.fromList
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
    = Textures
    | Sandbox
    | ObjectLoader
    | Light
    | Landscape


sceneOptions : Model -> Maybe Scene.Options
sceneOptions model =
    case Array.get model.currentSceneIndex model.scenes of
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


loadScene : Model -> ( Model, Cmd Msg )
loadScene model =
    case Array.get model.currentSceneIndex model.scenes of
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
