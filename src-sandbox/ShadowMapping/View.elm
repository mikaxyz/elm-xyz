module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (height, width)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import ShadowMapping.Model exposing (Model, Msg(..))
import ShadowMapping.Renderer
import WebGL
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.Material.Color
import XYZMika.XYZ.Material.Renderer
import XYZMika.XYZ.Scene
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Options


viewport : { width : number, height : number }
viewport =
    { width = 800
    , height = 600
    }


doc : Model -> Browser.Document Msg
doc model =
    { title = "ShadowMapping"
    , body = view model |> List.singleton
    }


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width viewport.width
        , height viewport.height
        , Dragon.dragEvents DragonMsg
        ]
        (XYZMika.XYZ.Scene.render
            [ Light.directional (vec3 -1 1 1) ]
            XYZMika.XYZ.Scene.Options.create
            viewport
            (vec2 0 0)
            model.theta
            Nothing
            (\_ -> Nothing)
            model.scene
            (Maybe.map XYZMika.XYZ.Material.Renderer.renderer
                >> Maybe.withDefault ShadowMapping.Renderer.renderer
            )
        )
