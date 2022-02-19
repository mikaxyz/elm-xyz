module XYZMika.XYZ exposing (toHtml)

import Html exposing (Attribute, Html)
import Html.Attributes
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material exposing (Renderer, ShadowMaps)
import XYZMika.XYZ.Material.DepthMap
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight exposing (SpotLight)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type alias ShadowMapBuffers =
    { shadowMap1 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap2 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap3 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap4 : Maybe ( WebGL.FrameBuffer, Mat4 )
    , shadowMap5 : Maybe ( WebGL.FrameBuffer, Mat4 )
    }


toHtml :
    List (Attribute msg)
    -> { width : Int, height : Int }
    -> List (Scene.Modifier objectId materialId)
    -> Renderer objectId materialId (Uniforms {})
    -> Scene objectId materialId
    -> Html msg
toHtml attributes viewport modifiers renderer scene =
    case frameBuffersForShadowMaps modifiers scene of
        Just frameBuffers ->
            WebGL.toHtmlWithFrameBuffers
                ([ frameBuffers.shadowMap1
                 , frameBuffers.shadowMap2
                 , frameBuffers.shadowMap3
                 , frameBuffers.shadowMap4
                 , frameBuffers.shadowMap5
                 ]
                    |> List.filterMap identity
                    |> List.map Tuple.first
                )
                [ WebGL.alpha True
                , WebGL.antialias
                , WebGL.depth 1
                , WebGL.clearColor (22 / 255.0) (22 / 255.0) (29 / 255.0) 1
                ]
                (Html.Attributes.width viewport.width
                    :: Html.Attributes.height viewport.height
                    :: attributes
                )
                (\textures ->
                    case List.head textures of
                        Just fallbackTexture ->
                            let
                                shadowMaps : ShadowMaps
                                shadowMaps =
                                    [ frameBuffers.shadowMap1
                                    , frameBuffers.shadowMap2
                                    , frameBuffers.shadowMap3
                                    , frameBuffers.shadowMap4
                                    , frameBuffers.shadowMap5
                                    ]
                                        |> List.foldl
                                            (\shadowMap ( acc, textures_, index ) ->
                                                let
                                                    attach :
                                                        Texture
                                                        -> ShadowMaps
                                                        -> Maybe ShadowMaps
                                                    attach tex x =
                                                        case ( index, shadowMap ) of
                                                            ( 0, Just ( _, mat ) ) ->
                                                                Just { x | shadowMap1 = Just { texture = tex, viewMatrix = mat } }

                                                            ( 1, Just ( _, mat ) ) ->
                                                                Just { x | shadowMap2 = Just { texture = tex, viewMatrix = mat } }

                                                            ( 2, Just ( _, mat ) ) ->
                                                                Just { x | shadowMap3 = Just { texture = tex, viewMatrix = mat } }

                                                            ( 3, Just ( _, mat ) ) ->
                                                                Just { x | shadowMap4 = Just { texture = tex, viewMatrix = mat } }

                                                            ( 4, Just ( _, mat ) ) ->
                                                                Just { x | shadowMap5 = Just { texture = tex, viewMatrix = mat } }

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
                                            ( { fallbackTexture = fallbackTexture
                                              , shadowMap1 = Nothing
                                              , shadowMap2 = Nothing
                                              , shadowMap3 = Nothing
                                              , shadowMap4 = Nothing
                                              , shadowMap5 = Nothing
                                              }
                                            , textures
                                            , 0
                                            )
                                        |> (\( shadowMaps_, _, _ ) -> shadowMaps_)
                            in
                            Scene.renderSimpleWithModifiers
                                modifiers
                                viewport
                                scene
                                (\name options -> renderer name (XYZMika.XYZ.Material.withShadowMaps shadowMaps options))

                        Nothing ->
                            []
                )

        Nothing ->
            WebGL.toHtml
                (Html.Attributes.width viewport.width
                    :: Html.Attributes.height viewport.height
                    :: attributes
                )
                (Scene.renderSimpleWithModifiers
                    modifiers
                    viewport
                    scene
                    renderer
                )


frameBuffersForShadowMaps : List (Scene.Modifier objectId materialId) -> Scene objectId materialId -> Maybe ShadowMapBuffers
frameBuffersForShadowMaps modifiers scene =
    let
        frameBufferAndViewMatrixForLight =
            frameBufferForSpotLight modifiers scene

        lightsInScene =
            scene
                |> Scene.withModifiers modifiers
                |> Scene.spotLights
                |> List.reverse
    in
    case lightsInScene of
        [] ->
            Nothing

        lights_ ->
            lights_
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

                                                3 ->
                                                    ( { acc | shadowMap4 = Just <| frameBufferAndViewMatrixForLight light_ }, index_ )

                                                4 ->
                                                    ( { acc | shadowMap5 = Just <| frameBufferAndViewMatrixForLight light_ }, index_ )

                                                _ ->
                                                    ( acc, index_ )

                                        Nothing ->
                                            ( acc, index_ )
                                )
                                ( ShadowMapBuffers Nothing Nothing Nothing Nothing Nothing, 0 )
                            |> Tuple.first
                   )
                |> Just


frameBufferForSpotLight :
    List (Scene.Modifier objectId materialId)
    -> Scene objectId materialId
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
                |> Scene.withCameraPosition (SpotLight.position spotLight)
                |> Scene.withCameraTarget (SpotLight.target spotLight)
                |> Scene.withPerspectiveProjection
                    { fov = min 180 (shadowMapData.fov + 10)
                    , near = shadowMapData.near
                    , far = shadowMapData.far
                    }

        camera =
            Scene.camera scene
                |> Camera.toMat4

        aspectRatio =
            1
    in
    ( WebGL.frameBuffer ( shadowMapData.resolution, shadowMapData.resolution )
        (Scene.renderSimpleWithModifiers
            modifiers
            { width = shadowMapData.resolution, height = shadowMapData.resolution }
            scene
            (always XYZMika.XYZ.Material.DepthMap.renderer)
        )
    , Mat4.mul (Scene.projectionMatrix aspectRatio scene) camera
    )
