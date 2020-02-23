module View exposing (doc, view)

import Asset
import Asset.Store
import Browser
import DDD.Scene as Scene
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model, Msg)
import WebGL
import WebGL.Texture exposing (Texture)


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm Web GL Experiment"
    , body =
        Asset.Store.texture Asset.Placeholder model.assets
            |> Maybe.map (\defaultTexture -> view defaultTexture model |> List.singleton)
            |> Maybe.withDefault []
    }


type alias Config =
    { width : Int
    , height : Int
    }


viewport =
    { width = 800
    , height = 600
    }


view : Texture -> Model -> Html msg
view defaultTexture model =
    WebGL.toHtml
        [ width viewport.width
        , height viewport.height
        ]
        (Scene.render
            defaultTexture
            viewport
            (Model.getDrag model)
            model.theta
            (Model.sceneOptions model)
            model.assets
            model.scene
        )
