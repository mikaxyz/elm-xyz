module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , ShadowMap
    , ShadowMaps
    , createOptions
    , directionalLights
    , lights
    , material
    , pointLightByIndex
    , shadowMaps
    , spotLightByIndex
    , toEntity
    , toEntityWithSettings
    , withLights
    , withShadowMaps
    )

import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight exposing (PointLight)
import XYZMika.XYZ.Scene.Light.SpotLight exposing (SpotLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias ShadowMap =
    { texture : Texture
    , viewMatrix : Mat4
    }


type alias ShadowMaps =
    { fallbackTexture : Texture
    , shadowMap1 : Maybe ShadowMap
    , shadowMap2 : Maybe ShadowMap
    , shadowMap3 : Maybe ShadowMap
    , shadowMap4 : Maybe ShadowMap
    , shadowMap5 : Maybe ShadowMap
    }


type Options
    = Options
        { lights : List Light
        , shadowMaps : Maybe ShadowMaps
        }


createOptions : Options
createOptions =
    Options
        { lights = []
        , shadowMaps = Nothing
        }


withLights : List Light -> Options -> Options
withLights lights_ (Options options) =
    Options { options | lights = lights_ }


withShadowMaps : ShadowMaps -> Options -> Options
withShadowMaps x (Options options) =
    Options { options | shadowMaps = Just x }


shadowMaps : Options -> Maybe ShadowMaps
shadowMaps (Options options) =
    options.shadowMaps


lights : Options -> List Light
lights (Options options) =
    options.lights


pointLightByIndex : Int -> Options -> Maybe PointLight
pointLightByIndex i (Options options) =
    options.lights
        |> List.filterMap Light.maybePointLight
        |> List.drop i
        |> List.head


spotLightByIndex : Int -> Options -> Maybe SpotLight
spotLightByIndex i (Options options) =
    options.lights
        |> List.reverse
        |> List.filterMap Light.maybeSpotLight
        |> List.drop i
        |> List.head


directionalLights : Options -> List DirectionalLight
directionalLights (Options options) =
    options.lights
        |> List.filterMap Light.maybeDirectionalLight


type alias Renderer objectId materialId uniforms =
    Maybe materialId
    -> Options
    -> uniforms
    -> Object objectId materialId
    -> Entity


type Material uniforms v
    = Material uniforms (VertexShader uniforms v) (FragmentShader uniforms v)


type VertexShader u v
    = VertexShader (Shader Vertex u v)


type FragmentShader u v
    = FragmentShader (Shader {} u v)


material : u -> Shader Vertex u v -> Shader {} u v -> Material u v
material u v f =
    Material u (VertexShader v) (FragmentShader f)


toEntity : Object objectId materialId -> Material uniforms v -> Entity
toEntity object (Material uniforms_ (VertexShader vShader) (FragmentShader fShader)) =
    case Object.glSetting object of
        Just glSetting ->
            WebGL.entityWith
                [ glSetting ]
                vShader
                fShader
                (Object.mesh object)
                uniforms_

        Nothing ->
            WebGL.entity
                vShader
                fShader
                (Object.mesh object)
                uniforms_


toEntityWithSettings : List WebGL.Settings.Setting -> Object objectId materialId -> Material uniforms v -> Entity
toEntityWithSettings settings object (Material uniforms_ (VertexShader vShader) (FragmentShader fShader)) =
    WebGL.entityWith
        settings
        vShader
        fShader
        (Object.mesh object)
        uniforms_
