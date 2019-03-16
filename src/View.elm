module View exposing (doc, view)

import Browser
import DDD.Scene as Scene
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model, Msg)
import WebGL


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm Web GL Experiment"
    , body = view model |> List.singleton
    }


type alias Config =
    { width : Int
    , height : Int
    }


viewport =
    { width = 800
    , height = 600
    }


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width viewport.width
        , height viewport.height
        ]
        (Scene.render
            viewport
            model.theta
            (Model.sceneOptions model)
            model.scene
        )
