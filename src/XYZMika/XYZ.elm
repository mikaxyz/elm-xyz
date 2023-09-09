module XYZMika.XYZ exposing
    ( View
    , Viewport
    , toHtml
    , view
    , withDefaultLights
    , withModifiers
    , withRenderOptions
    , withSceneOptions
    )

import Html exposing (Attribute, Html)
import Html.Attributes
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.WebGL
import XYZMika.XYZ.Material exposing (Renderer, ShadowMaps)
import XYZMika.XYZ.Material.DepthMap
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light exposing (Light)
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight exposing (SpotLight)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Options as SceneOptions
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type View objectId materialId
    = View (Config objectId materialId)


type alias Viewport =
    { width : Int, height : Int }


type alias Config objectId materialId =
    { viewport : Viewport
    , renderer : Renderer objectId materialId (Uniforms {})
    , modifiers : List (Scene.Modifier objectId materialId)
    , defaultLights : List Light
    , sceneOptions : SceneOptions.Options
    , renderOptions : Graph ( Int, Object objectId materialId ) -> Maybe Scene.GraphRenderOptions
    }


view :
    Viewport
    -> Renderer objectId materialId (Uniforms {})
    -> View objectId materialId
view viewport renderer =
    View
        { viewport = viewport
        , renderer = renderer
        , modifiers = []
        , defaultLights = []
        , sceneOptions = SceneOptions.create
        , renderOptions = \_ -> Nothing
        }


withModifiers :
    List (Scene.Modifier objectId materialId)
    -> View objectId materialId
    -> View objectId materialId
withModifiers x (View config) =
    View { config | modifiers = x }


withSceneOptions :
    SceneOptions.Options
    -> View objectId materialId
    -> View objectId materialId
withSceneOptions x (View config) =
    View { config | sceneOptions = x }


withRenderOptions :
    (Graph ( Int, Object objectId materialId ) -> Maybe Scene.GraphRenderOptions)
    -> View objectId materialId
    -> View objectId materialId
withRenderOptions x (View config) =
    View { config | renderOptions = x }


withDefaultLights :
    List Light
    -> View objectId materialId
    -> View objectId materialId
withDefaultLights x (View config) =
    View { config | defaultLights = x }


toHtml :
    List (Attribute msg)
    -> Scene objectId materialId
    -> View objectId materialId
    -> Html msg
toHtml attributes scene (View ({ viewport, modifiers, renderer } as config)) =
    case
        XYZMika.WebGL.supportsFrameBuffers
            (\_ -> frameBuffersForShadowMaps modifiers scene)
            (\_ -> Nothing)
    of
        Just frameBuffers ->
            XYZMika.WebGL.toHtmlWithFrameBuffers
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
                            Scene.render
                                config.defaultLights
                                config.modifiers
                                config.sceneOptions
                                config.viewport
                                config.renderOptions
                                scene
                                (\name options -> renderer name (XYZMika.XYZ.Material.withShadowMaps shadowMaps options))

                        Nothing ->
                            []
                )

        Nothing ->
            let
                lightsInScene : List SpotLight
                lightsInScene =
                    scene
                        |> Scene.withModifiers modifiers
                        |> Scene.spotLights
                        |> List.reverse
            in
            WebGL.toHtml
                (Html.Attributes.width viewport.width
                    :: Html.Attributes.height viewport.height
                    :: attributes
                )
                (Scene.render
                    config.defaultLights
                    config.modifiers
                    config.sceneOptions
                    config.viewport
                    config.renderOptions
                    scene
                    renderer
                )


type alias ShadowMapBuffers =
    { shadowMap1 : Maybe ( XYZMika.WebGL.FrameBuffer, Mat4 )
    , shadowMap2 : Maybe ( XYZMika.WebGL.FrameBuffer, Mat4 )
    , shadowMap3 : Maybe ( XYZMika.WebGL.FrameBuffer, Mat4 )
    , shadowMap4 : Maybe ( XYZMika.WebGL.FrameBuffer, Mat4 )
    , shadowMap5 : Maybe ( XYZMika.WebGL.FrameBuffer, Mat4 )
    }


frameBuffersForShadowMaps : List (Scene.Modifier objectId materialId) -> Scene objectId materialId -> Maybe ShadowMapBuffers
frameBuffersForShadowMaps modifiers scene =
    let
        frameBufferAndViewMatrixForLight =
            frameBufferForSpotLight modifiers scene

        lightsInScene : List SpotLight
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
    -> ( XYZMika.WebGL.FrameBuffer, Mat4 )
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
    ( XYZMika.WebGL.frameBuffer ( shadowMapData.resolution, shadowMapData.resolution )
        (renderSimpleWithModifiers
            modifiers
            { width = shadowMapData.resolution, height = shadowMapData.resolution }
            scene
            (always XYZMika.XYZ.Material.DepthMap.renderer)
        )
    , Mat4.mul (Scene.projectionMatrix aspectRatio scene) camera
    )


renderSimpleWithModifiers :
    List (Scene.Modifier objectId materialId)
    -> { width : Int, height : Int }
    -> Scene objectId materialId
    -> Renderer objectId materialId (Uniforms {})
    -> List Entity
renderSimpleWithModifiers modifiers viewport scene renderer =
    Scene.render
        []
        modifiers
        SceneOptions.create
        viewport
        (\_ -> Nothing)
        scene
        renderer
