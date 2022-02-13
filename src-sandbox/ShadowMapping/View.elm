module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import ShadowMapping.Material.Advanced
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import WebGL
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.DepthMap
import XYZMika.XYZ.Material.Renderer
import XYZMika.XYZ.Scene
import XYZMika.XYZ.Scene.Camera
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight exposing (SpotLight)


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


type alias ShadowMapBuffers =
    { shadowMap1 : ( Maybe WebGL.FrameBuffer, Mat4 )
    , shadowMap2 : ( Maybe WebGL.FrameBuffer, Mat4 )
    }


view : Model -> XYZMika.XYZ.Scene.Scene XYZMika.XYZ.Material.Renderer.Name -> Html Msg
view model scene =
    let
        frameBuffers : ShadowMapBuffers
        frameBuffers =
            scene
                |> XYZMika.XYZ.Scene.withModifiers (Model.modifiers model)
                |> XYZMika.XYZ.Scene.spotLights
                |> List.filterMap
                    (\spotlight ->
                        SpotLight.shadowMapData spotlight
                            |> Maybe.map (\shadowMapData -> ( shadowMapData, spotlight ))
                    )
                |> List.map (frameBufferForSpotLight (Model.modifiers model) scene)
                |> (\x ->
                        case x of
                            [ ( fb, mat ) ] ->
                                ShadowMapBuffers ( Just fb, mat ) ( Nothing, Mat4.identity )

                            [ ( fb1, mat1 ), ( fb2, mat2 ) ] ->
                                ShadowMapBuffers ( Just fb1, mat1 ) ( Just fb2, mat2 )

                            _ ->
                                ShadowMapBuffers ( Nothing, Mat4.identity ) ( Nothing, Mat4.identity )
                   )
    in
    WebGL.toHtmlWithFrameBuffers
        ([ frameBuffers.shadowMap1, frameBuffers.shadowMap2 ] |> List.filterMap Tuple.first)
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
                    let
                        shadowMaps =
                            case textures of
                                t1 :: [] ->
                                    Just
                                        { shadowMap1 =
                                            { texture = t1
                                            , viewMatrix = Tuple.second frameBuffers.shadowMap1
                                            }
                                        , shadowMap2 = Nothing
                                        }

                                t1 :: t2 :: [] ->
                                    Just
                                        { shadowMap1 =
                                            { texture = t1
                                            , viewMatrix = Tuple.second frameBuffers.shadowMap1
                                            }
                                        , shadowMap2 =
                                            Just
                                                { texture = t2
                                                , viewMatrix = Tuple.second frameBuffers.shadowMap2
                                                }
                                        }

                                _ ->
                                    Nothing
                    in
                    case shadowMaps of
                        Just shadowMaps_ ->
                            material
                                |> Maybe.map XYZMika.XYZ.Material.Renderer.renderer
                                |> Maybe.withDefault
                                    (ShadowMapping.Material.Advanced.renderer shadowMaps_)

                        Nothing ->
                            XYZMika.XYZ.Material.Advanced.renderer
                )
        )


frameBufferForSpotLight :
    List XYZMika.XYZ.Scene.Modifier
    -> XYZMika.XYZ.Scene.Scene XYZMika.XYZ.Material.Renderer.Name
    ->
        ( { resolution : Int
          , fov : Float
          , near : Float
          , far : Float
          }
        , SpotLight
        )
    -> ( WebGL.FrameBuffer, Mat4 )
frameBufferForSpotLight modifiers originalScene ( shadowMapData, spotLight ) =
    let
        scene =
            originalScene
                |> XYZMika.XYZ.Scene.withCameraPosition (SpotLight.position spotLight)
                |> XYZMika.XYZ.Scene.withCameraTarget (SpotLight.target spotLight)
                |> XYZMika.XYZ.Scene.withPerspectiveProjection
                    { fov = min 180 (shadowMapData.fov + 10)
                    , near = shadowMapData.near
                    , far = shadowMapData.far
                    }

        camera =
            XYZMika.XYZ.Scene.camera scene
                |> XYZMika.XYZ.Scene.Camera.toMat4

        aspectRatio =
            1
    in
    ( WebGL.frameBuffer ( shadowMapData.resolution, shadowMapData.resolution )
        (XYZMika.XYZ.Scene.renderSimpleWithModifiers
            modifiers
            { width = shadowMapData.resolution, height = shadowMapData.resolution }
            scene
            (always XYZMika.XYZ.Material.DepthMap.renderer)
        )
    , Mat4.mul (XYZMika.XYZ.Scene.projectionMatrix aspectRatio scene) camera
    )
