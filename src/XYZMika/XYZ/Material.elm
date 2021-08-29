module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , addLight
    , createOptions
    , directionalLights
    , fragmentShader
    , lights
    , material
    , pointLightByIndex
    , toEntity
    , uniforms
    , vertexShader
    , withLights
    )

import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight exposing (PointLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type Options
    = Options
        { lights : List Light
        }


lights : Options -> List Light
lights (Options options) =
    options.lights


withLights : List Light -> Options -> Options
withLights lights_ (Options options) =
    Options { options | lights = lights_ }


addLight : Light -> Options -> Options
addLight x (Options options) =
    Options { options | lights = x :: options.lights }


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


createOptions : Options
createOptions =
    Options { lights = [] }


type alias Renderer materialId uniforms =
    Maybe materialId
    -> Options
    -> Texture
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


vertexShader : Material uniforms v -> Shader Vertex uniforms v
vertexShader (Material _ (VertexShader x) _) =
    x


fragmentShader : Material uniforms v -> Shader {} uniforms v
fragmentShader (Material _ _ (FragmentShader x)) =
    x


uniforms : Material uniforms v -> uniforms
uniforms (Material x _ _) =
    x


toEntity : Object materialId -> Material uniforms v -> Entity
toEntity object mat =
    case Object.glSetting object of
        Just glSetting ->
            WebGL.entityWith
                [ glSetting ]
                (vertexShader mat)
                (fragmentShader mat)
                (Object.mesh object)
                (uniforms mat)

        Nothing ->
            WebGL.entity
                (vertexShader mat)
                (fragmentShader mat)
                (Object.mesh object)
                (uniforms mat)
