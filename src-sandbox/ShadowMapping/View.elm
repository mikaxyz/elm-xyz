module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html)
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import XYZMika.Dragon as Dragon
import XYZMika.XYZ
import XYZMika.XYZ.Material.Advanced


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
        (always XYZMika.XYZ.Material.Advanced.renderer)
        model.scene
