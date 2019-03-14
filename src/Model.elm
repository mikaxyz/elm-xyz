module Model exposing (Model, Msg(..), init, nextScene)

import DDD.Scene exposing (Scene)
import Math.Vector2 exposing (Vec2)
import Scenes.Landscape
import Scenes.Sandbox


type ActiveScene
    = Sandbox
    | Landscape


nextScene model =
    case model.currentScene of
        Sandbox ->
            { model
                | currentScene = Landscape
                , scene = Scenes.Landscape.init
            }

        Landscape ->
            { model
                | currentScene = Sandbox
                , scene = Scenes.Sandbox.init
            }


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2
    | KeyPressed String


type alias Model =
    { theta : Float
    , drag : Maybe { from : Vec2, to : Vec2 }
    , scene : Scene
    , currentScene : ActiveScene
    }


init : Model
init =
    { theta = 0
    , drag = Nothing
    , scene = Scenes.Sandbox.init
    , currentScene = Sandbox
    }
