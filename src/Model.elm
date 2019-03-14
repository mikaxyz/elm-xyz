module Model exposing (Model, Msg(..), init, nextScene)

import DDD.Scene exposing (Scene)
import Math.Vector2 exposing (Vec2)
import Scenes.Landscape
import Scenes.ObjectLoader
import Scenes.Sandbox


type ActiveScene
    = Sandbox
    | Landscape
    | ObjectLoader


withScene : ActiveScene -> Model -> ( Model, Cmd Msg )
withScene scene model =
    case scene of
        Landscape ->
            ( { model
                | currentScene = Landscape
                , scene = Scenes.Landscape.init
              }
            , Cmd.none
            )

        ObjectLoader ->
            ( { model
                | currentScene = ObjectLoader
                , scene = Scenes.ObjectLoader.init
              }
            , Scenes.ObjectLoader.getObj GotObj
            )

        Sandbox ->
            ( { model
                | currentScene = Sandbox
                , scene = Scenes.Sandbox.init
              }
            , Cmd.none
            )


nextScene : Model -> ( Model, Cmd Msg )
nextScene model =
    case model.currentScene of
        Sandbox ->
            model |> withScene Landscape

        Landscape ->
            model |> withScene ObjectLoader

        ObjectLoader ->
            model |> withScene Sandbox


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
    , currentScene : ActiveScene
    }


init : ( Model, Cmd Msg )
init =
    { theta = 0
    , drag = Nothing
    , scene = Scenes.Sandbox.init
    , currentScene = Sandbox
    }
        |> withScene ObjectLoader
