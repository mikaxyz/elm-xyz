module ShadowMapping.View exposing (doc, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import ShadowMapping.Material.Advanced
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.Dragon as Dragon
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
    { shadowMap1 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap2 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap3 : Maybe ( WebGL.FrameBuffer, Mat4 )
    }


view : Model -> XYZMika.XYZ.Scene.Scene XYZMika.XYZ.Material.Renderer.Name -> Html Msg
view model scene =
    let
        frameBufferAndViewMatrixForLight =
            frameBufferForSpotLight (Model.modifiers model) scene

        frameBuffers : ShadowMapBuffers
        frameBuffers =
            scene
                |> XYZMika.XYZ.Scene.withModifiers (Model.modifiers model)
                |> XYZMika.XYZ.Scene.spotLights
                |> List.reverse
                |> List.map
                    (\spotlight ->
                        SpotLight.shadowMapData spotlight
                            |> Maybe.map (\shadowMapData -> ( shadowMapData, spotlight ))
                    )
                |> (\lights ->
                        lights
                            |> List.foldl
                                (\light ( acc, index ) ->
                                    let
                                        index_ =
                                            index + 1
                                    in
                                    case light of
                                        Just light_ ->
                                            case index of
                                                0 ->
                                                    ( { acc | shadowMap1 = Just <| frameBufferAndViewMatrixForLight light_ }, index_ )

                                                1 ->
                                                    ( { acc | shadowMap2 = Just <| frameBufferAndViewMatrixForLight light_ }, index_ )

                                                2 ->
                                                    ( { acc | shadowMap3 = Just <| frameBufferAndViewMatrixForLight light_ }, index_ )

                                                _ ->
                                                    ( acc, index_ )

                                        Nothing ->
                                            ( acc, index_ )
                                )
                                ( ShadowMapBuffers Nothing Nothing Nothing, 0 )
                            |> Tuple.first
                   )
    in
    WebGL.toHtmlWithFrameBuffers
        ([ frameBuffers.shadowMap1, frameBuffers.shadowMap2, frameBuffers.shadowMap3 ]
            |> List.filterMap identity
            |> List.map Tuple.first
        )
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.clearColor (22 / 255.0) (22 / 255.0) (29 / 255.0) 1
        ]
        [ width viewport.width
        , height viewport.height
        , Dragon.dragEvents DragonMsg
        ]
        (\textures ->
            case List.head textures of
                Just texture1 ->
                    XYZMika.XYZ.Scene.renderSimpleWithModifiers
                        (Model.modifiers model)
                        viewport
                        scene
                        (\material ->
                            [ frameBuffers.shadowMap1, frameBuffers.shadowMap2, frameBuffers.shadowMap3 ]
                                |> List.foldl
                                    (\shadowMap ( acc, textures_, index ) ->
                                        let
                                            attach :
                                                Texture
                                                -> ShadowMapping.Material.Advanced.ShadowMaps
                                                -> Maybe ShadowMapping.Material.Advanced.ShadowMaps
                                            attach tex x =
                                                case ( index, shadowMap ) of
                                                    ( 0, Just ( _, mat ) ) ->
                                                        Just { x | shadowMap1 = Just { texture = tex, viewMatrix = mat } }

                                                    ( 1, Just ( _, mat ) ) ->
                                                        Just { x | shadowMap2 = Just { texture = tex, viewMatrix = mat } }

                                                    ( 2, Just ( _, mat ) ) ->
                                                        Just { x | shadowMap3 = Just { texture = tex, viewMatrix = mat } }

                                                    _ ->
                                                        Nothing
                                        in
                                        case textures_ of
                                            tex :: rest ->
                                                case attach tex acc of
                                                    Just updated ->
                                                        ( updated, rest, index + 1 )

                                                    Nothing ->
                                                        ( acc, tex :: rest, index + 1 )

                                            [] ->
                                                ( acc, [], index + 1 )
                                    )
                                    ( { shadowMap1 = Nothing
                                      , shadowMap2 = Nothing
                                      , shadowMap3 = Nothing
                                      }
                                    , textures
                                    , 0
                                    )
                                |> (\( acc, _, _ ) -> acc)
                                |> ShadowMapping.Material.Advanced.renderer texture1
                        )

                Nothing ->
                    []
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
