module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as D
import Keyboard
import Math.Vector2 as Vec2 exposing (Vec2)
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
    let
        vectorDecoder : D.Decoder Vec2
        vectorDecoder =
            D.map2 Vec2.vec2
                (D.field "x" D.float)
                (D.field "y" D.float)
    in
    Sub.batch
        [ Keyboard.subscriptions { tagger = KeyboardMsg, keyDown = OnKeyDown }
        , Dragon.subscriptions model.dragon |> Sub.map DragonMsg
        , Browser.Events.onAnimationFrameDelta Animate
        , Browser.Events.onResize (\_ _ -> OnResize)
        , Browser.Events.onMouseDown (vectorDecoder |> D.map OnMouseUp)
        ]
