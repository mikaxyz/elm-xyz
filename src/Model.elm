module Model exposing
    ( Model
    , Msg(..)
    , init
    , nextScene
    , prevScene
    )

import Array exposing (Array)
import DDD.Scene exposing (Scene)
import Math.Vector2 exposing (Vec2)
import Scenes.Landscape
import Scenes.ObjectLoader
import Scenes.Sandbox


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyPressed String
    | GotObj String


type alias Model =
    { theta : Float
    , drag : Maybe { from : Vec2, to : Vec2 }
    , scene : Scene
    , scenes : Array ActiveScene
    , currentSceneIndex : Int
    }


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , drag = Nothing
    , scene = Scenes.Sandbox.init
    , scenes =
        Array.fromList
            [ Sandbox
            , ObjectLoader
            , Landscape
            ]
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
    | Landscape
    | ObjectLoader


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
            , Scenes.ObjectLoader.getObj GotObj "obj/monkey.obj"
            )

        Just Landscape ->
            ( { model
                | scene = Scenes.Landscape.init
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )
