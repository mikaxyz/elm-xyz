module View exposing (doc, view)

import Asset
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model, Msg)
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Scene as Scene


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm Web GL Experiment"
    , body =
        AssetStore.texture Asset.Empty model.assets
            |> Maybe.map (\defaultTexture -> view defaultTexture model |> List.singleton)
            |> Maybe.withDefault []
    }


type alias Config =
    { width : Int
    , height : Int
    }


viewport =
    { width = 1600
    , height = 800
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
            model.scene
        )
