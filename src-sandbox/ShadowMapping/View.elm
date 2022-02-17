module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html)
import Math.Matrix4 exposing (Mat4)
import ShadowMapping.Material.Advanced
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import WebGL
import XYZMika.Dragon as Dragon
import XYZMika.XYZ


doc : Model -> Browser.Document Msg
doc model =
    { title = "ShadowMapping"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    XYZMika.XYZ.toHtml [ Dragon.dragEvents DragonMsg ]
        { width = 800
        , height = 600
        }
        (Model.modifiers model)
        (\_ -> ShadowMapping.Material.Advanced.renderer)
        model.scene
