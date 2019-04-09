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
import DDD.Scene as Scene exposing (Scene)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Scenes.Landscape
import Scenes.Light
import Scenes.ObjectLoader
import Scenes.Sandbox


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyPressed String
    | GotObj ( { scale : Float, color : Vec3 }, Vec3, String )


type alias Model =
    { theta : Float
    , dragger : Maybe { from : Vec2, to : Vec2 }
    , drag : Vec2
    , scene : Scene
    , scenes : Array ActiveScene
    , currentSceneIndex : Int
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
    , scenes = [ Landscape, ObjectLoader, Light, Sandbox ] |> Array.fromList
    , currentSceneIndex = 0
    }
        |> loadScene


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
    = Sandbox
    | ObjectLoader
    | Light
    | Landscape


sceneOptions : Model -> Maybe Scene.Options
sceneOptions model =
    case Array.get model.currentSceneIndex model.scenes of
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
                    (vec3 0 -0.7 -0.5)
                    "obj/deer.obj"
                    GotObj
                , Scenes.ObjectLoader.getObj
                    { scale = 0.3, color = vec3 0.5 0.5 1 }
                    (vec3 -0.5 0.5 0)
                    "obj/monkey.obj"
                    GotObj
                , Scenes.ObjectLoader.getObj
                    { scale = 0.001, color = vec3 1 1 0.5 }
                    (vec3 0 -0.7 0.5)
                    "obj/cat.obj"
                    GotObj
                , Scenes.ObjectLoader.getObj
                    { scale = 0.001, color = vec3 0.5 1 1 }
                    (vec3 0 -0.7 0)
                    "obj/wolf.obj"
                    GotObj
                , Scenes.ObjectLoader.getObj
                    { scale = 0.3, color = vec3 0.5 1 0.5 }
                    (vec3 -0.7 -0.6 0)
                    "obj/cube.obj"
                    GotObj
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
