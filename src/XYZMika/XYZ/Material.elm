module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , defaultOptions
    , fragmentShader
    , material
    , setDirectionalLight
    , setPointLight
    , toEntity
    , uniforms
    , vertexShader
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Light as Light exposing (PointLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Options =
    { lights :
        { directional : Vec3
        , point1 : PointLight
        , point2 : PointLight
        }
    }


defaultOptions : Options
defaultOptions =
    { lights =
        { directional = Vec3.fromRecord { x = -1, y = -1, z = -3 }
        , point1 =
            Light.pointLight (vec3 2 2 3)
                |> Light.withIntensity 0.8
                |> Light.withColor (vec3 0.8 0.6 0.5)
        , point2 =
            Light.pointLight (vec3 -1 3 -2)
                |> Light.withIntensity 0.3
                |> Light.withColor (vec3 0.1 0.3 1)
        }
    }


setDirectionalLight : Vec3 -> Options -> Options
setDirectionalLight x options =
    (\lights -> { options | lights = { lights | directional = x } }) options.lights


setPointLight : Vec3 -> Options -> Options
setPointLight x options =
    (\lights -> { options | lights = { lights | point1 = lights.point1 |> Light.withPosition x } }) options.lights


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
