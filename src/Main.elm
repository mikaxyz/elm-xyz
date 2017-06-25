module Main exposing (..)

import Html exposing (Html, program)
import AnimationFrame
import Msg exposing (Msg(..))
import View exposing (view, initModel, Model)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> AnimationFrame.diffs Animate)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }, Cmd.none )
