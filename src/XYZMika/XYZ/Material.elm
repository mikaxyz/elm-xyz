module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , createOptions
    , directionalLights
    , lights
    , material
    , pointLightByIndex
    , toEntity
    , toEntityWithSettings
    , withLights
    )

import WebGL exposing (Entity, Shader)
import WebGL.Settings
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight exposing (PointLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type Options
    = Options
        { lights : List Light
        }


createOptions : Options
createOptions =
    Options { lights = [] }


withLights : List Light -> Options -> Options
withLights lights_ (Options options) =
    Options { options | lights = lights_ }


lights : Options -> List Light
lights (Options options) =
    options.lights


pointLightByIndex : Int -> Options -> Maybe PointLight
pointLightByIndex i (Options options) =
    options.lights
        |> List.filterMap Light.maybePointLight
        |> List.drop i
        |> List.head


directionalLights : Options -> List DirectionalLight
directionalLights (Options options) =
    options.lights
        |> List.filterMap Light.maybeDirectionalLight


type alias Renderer materialId uniforms =
    Maybe materialId
    -> Options
    -> uniforms
    -> Object materialId
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


toEntity : Object materialId -> Material uniforms v -> Entity
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


toEntityWithSettings : List WebGL.Settings.Setting -> Object materialId -> Material uniforms v -> Entity
toEntityWithSettings settings object (Material uniforms_ (VertexShader vShader) (FragmentShader fShader)) =
    WebGL.entityWith
        settings
        vShader
        fShader
        (Object.mesh object)
        uniforms_
