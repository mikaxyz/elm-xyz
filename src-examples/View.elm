module View exposing (doc, view)

import Asset
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Model exposing (Model, Msg)
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Object exposing (Object)


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
            renderer
        )


renderer :
    Maybe (XYZMika.XYZ.Material.Id Material.Name)
    -> Texture
    -> { u | perspective : Mat4, camera : Mat4, worldMatrix : Mat4, uColor : Vec3 }
    -> Object Material.Name
    -> WebGL.Entity
renderer name =
    case name of
        Just (XYZMika.XYZ.Material.Id materialName) ->
            Material.renderer materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer
