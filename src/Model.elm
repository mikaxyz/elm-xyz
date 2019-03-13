module Model exposing (Model, Msg(..), init)

import DDD.Data.Node exposing (Node(..))
import DDD.Scene as Scene exposing (Scene)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2


type alias Model =
    { theta : Float
    , drag : Maybe { from : Vec2, to : Vec2 }
    , scene : Scene
    }


init : Model
init =
    { theta = 0
    , drag = Nothing
    , scene = Scene.init
    }
