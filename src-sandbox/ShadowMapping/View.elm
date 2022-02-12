module ShadowMapping.View exposing (doc, lightMap, view)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (height, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import ShadowMapping.Assets as Asset
import ShadowMapping.Material.Advanced
import ShadowMapping.Model exposing (Model, Msg(..))
import ShadowMapping.Scene as Scene
import WebGL
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.AssetStore as AssetStore
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
        [ view model
        ]
    }


view : Model -> Html Msg
view model =
    Maybe.map3
        (\mesh diffuse normal -> { mesh = mesh, diffuse = diffuse, normal = normal })
        (AssetStore.verticesIndexed Asset.SneakerXyz model.assets)
        (AssetStore.texture Asset.SneakerDiffuse model.assets)
        (AssetStore.texture Asset.SneakerNormal model.assets)
        |> Maybe.map
            (\assets ->
                model.scene
                    |> XYZMika.XYZ.Scene.map
                        (\_ ->
                            Scene.graph model.theta Nothing model.objectPosition assets
                        )
                    |> view_ model
            )
        |> Maybe.withDefault (text "Loading...")


view_ model scene =
    let
        ( frameBuffer, shadowMapTransforms ) =
            lightMap model.theta scene
    in
    WebGL.toHtmlWithFrameBuffers
        [ frameBuffer ]
        [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 ]
        [ width viewport.width
        , height viewport.height
        , Dragon.dragEvents DragonMsg
        ]
        (\textures ->
            let
                shadowMap =
                    List.head textures
            in
            XYZMika.XYZ.Scene.renderSimple
                viewport
                scene
                (\material ->
                    case shadowMap of
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


lightMap :
    Float
    -> XYZMika.XYZ.Scene.Scene XYZMika.XYZ.Material.Renderer.Name
    ->
        ( WebGL.FrameBuffer
        , { perspectiveMatrix : Mat4
          , cameraMatrix : Mat4
          , modelMatrix : Mat4
          }
        )
lightMap theta scene_ =
    let
        viewport_ : { width : number, height : number }
        viewport_ =
            { width = 640
            , height = 640
            }

        lightPosition =
            Scene.pointLightPosition theta

        scene =
            scene_
                |> XYZMika.XYZ.Scene.withCameraPosition lightPosition
                |> XYZMika.XYZ.Scene.withPerspectiveProjection { fov = 70, near = 0.01, far = 100 }

        camera =
            XYZMika.XYZ.Scene.camera scene
                |> XYZMika.XYZ.Scene.Camera.toMat4

        aspectRatio =
            toFloat viewport_.width / toFloat viewport_.height

        p =
            Mat4.makeTranslate lightPosition
    in
    ( WebGL.frameBuffer ( viewport_.width, viewport_.height )
        (XYZMika.XYZ.Scene.renderSimple
            viewport_
            scene
            (always XYZMika.XYZ.Material.DepthMap.renderer)
         --(always XYZMika.XYZ.Material.Color.renderer)
        )
    , { perspectiveMatrix = XYZMika.XYZ.Scene.projectionMatrix aspectRatio scene
      , cameraMatrix = camera
      , modelMatrix = p
      }
    )
