module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , addLight
    , defaultOptions
    , directionalLights
    , fragmentShader
    , material
    , pointLightByIndex
    , toEntity
    , uniforms
    , vertexShader
    )

import Math.Vector3 exposing (vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight exposing (PointLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Options =
    { lights : List Light
    }


addLight : Light -> Options -> Options
addLight x options =
    { options | lights = x :: options.lights }


pointLightByIndex : Int -> Options -> Maybe PointLight
pointLightByIndex i options =
    options.lights
        |> List.filterMap Light.maybePointLight
        |> List.drop i
        |> List.head


directionalLights : Options -> List DirectionalLight
directionalLights options =
    options.lights
        |> List.filterMap Light.maybeDirectionalLight


defaultOptions : Options
defaultOptions =
    { lights = [ Light.directional (vec3 0 1 3) ] }


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
