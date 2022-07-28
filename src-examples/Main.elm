module Main exposing (main)

import Browser
import Browser.Events
import Keyboard
import Model exposing (Model, Msg(..))
import Update exposing (update)
import View
import XYZMika.Dragon as Dragon


main : Program () Model Msg
main =
    Browser.document
        { init = always Model.init
        , view = View.doc
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.subscriptions { tagger = KeyboardMsg, keyDown = OnKeyDown }
        , Dragon.subscriptions model.dragon |> Sub.map DragonMsg
        , Browser.Events.onAnimationFrameDelta Animate
        , Browser.Events.onResize (\_ _ -> OnResize)
        ]
