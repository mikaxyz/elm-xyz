module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import ShadowMapping.Material.Advanced
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import ShadowMapping.Scene as Scene
import WebGL
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.DepthMap
import XYZMika.XYZ.Material.Renderer
import XYZMika.XYZ.Scene
import XYZMika.XYZ.Scene.Camera


viewport : { width : number, height : number }
viewport =
    { width = 800
    , height = 600
    }


doc : Model -> Browser.Document Msg
doc model =
    { title = "ShadowMapping"
    , body =
        [ view model model.scene
        ]
    }


view : Model -> XYZMika.XYZ.Scene.Scene XYZMika.XYZ.Material.Renderer.Name -> Html Msg
view model scene =
    let
        ( frameBuffer, shadowMapTransforms ) =
            shadowMap model
    in
    WebGL.toHtmlWithFrameBuffers
        [ frameBuffer ]
        [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 ]
        [ width viewport.width
        , height viewport.height
        , Dragon.dragEvents DragonMsg
        ]
        (\textures ->
            XYZMika.XYZ.Scene.renderSimpleWithModifiers
                (Model.modifiers model)
                viewport
                scene
                (\material ->
                    case List.head textures of
                        Just texture ->
                            material
                                |> Maybe.map XYZMika.XYZ.Material.Renderer.renderer
                                |> Maybe.withDefault
                                    (ShadowMapping.Material.Advanced.renderer
                                        { texture = texture
                                        , perspectiveMatrix = shadowMapTransforms.perspectiveMatrix
                                        , cameraMatrix = shadowMapTransforms.cameraMatrix
                                        , modelMatrix = shadowMapTransforms.modelMatrix
                                        }
                                    )

                        Nothing ->
                            XYZMika.XYZ.Material.Advanced.renderer
                )
        )


shadowMap :
    Model
    ->
        ( WebGL.FrameBuffer
        , { perspectiveMatrix : Mat4
          , cameraMatrix : Mat4
          , modelMatrix : Mat4
          }
        )
shadowMap model =
    let
        viewport_ : { width : number, height : number }
        viewport_ =
            { width = 640
            , height = 640
            }

        scene =
            model.scene
                |> XYZMika.XYZ.Scene.withCameraPosition Scene.spotLightPosition
                |> XYZMika.XYZ.Scene.withPerspectiveProjection { fov = 70, near = 0.01, far = 100 }

        camera =
            XYZMika.XYZ.Scene.camera scene
                |> XYZMika.XYZ.Scene.Camera.toMat4

        aspectRatio =
            toFloat viewport_.width / toFloat viewport_.height

        p =
            Mat4.makeTranslate Scene.spotLightPosition
    in
    ( WebGL.frameBuffer ( viewport_.width, viewport_.height )
        (XYZMika.XYZ.Scene.renderSimpleWithModifiers
            (Model.modifiers model)
            viewport_
            scene
            (always XYZMika.XYZ.Material.DepthMap.renderer)
        )
    , { perspectiveMatrix = XYZMika.XYZ.Scene.projectionMatrix aspectRatio scene
      , cameraMatrix = camera
      , modelMatrix = p
      }
    )
