module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as D
import Math.Vector2 as Vec2 exposing (Vec2)
import Model exposing (Model, Msg(..))
import Update exposing (update)
import View


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

        drags =
            case model.drag of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (vectorDecoder |> D.map Drag)
                        , Browser.Events.onMouseUp (vectorDecoder |> D.map DragEnd)
                        ]

                Nothing ->
                    Browser.Events.onMouseDown (vectorDecoder |> D.map DragStart)
    in
    Sub.batch
        [ drags
        , Browser.Events.onAnimationFrameDelta Animate
        , Browser.Events.onKeyPress (D.field "key" D.string) |> Sub.map KeyPressed
        ]
