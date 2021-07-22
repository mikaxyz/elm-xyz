module View exposing (doc, view)

import Asset
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Model exposing (Model, Msg)
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


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
    model.scene
        |> Maybe.map (sceneView defaultTexture model)
        |> Maybe.withDefault (text "")


sceneView : Texture -> Model -> Scene Material.Name -> Html msg
sceneView defaultTexture model scene =
    WebGL.toHtml
        [ width viewport.width
        , height viewport.height
        ]
        (Scene.render
            defaultTexture
            model.renderOptions
            viewport
            (Model.getDrag model)
            model.theta
            (Model.sceneOptions model)
            (scene |> Scene.withRendererOptions model.rendererOptions)
            renderer
        )


renderer :
    Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Texture
    -> Uniforms u
    -> Object Material.Name
    -> WebGL.Entity
renderer name =
    case name of
        Just materialName ->
            Material.renderer materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer
