module XYZMika.XYZ.Material exposing
    ( Material
    , Options
    , Renderer
    , defaultOptions
    , fragmentShader
    , material
    , setDirectionalLight
    , uniforms
    , vertexShader
    )

import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Object exposing (Object)


type alias Options =
    { lights : { directional : Vec3 }
    }


defaultOptions : { lights : { directional : Vec3 } }
defaultOptions =
    { lights = { directional = Vec3.fromRecord { x = -1, y = -0.7, z = -0.3 } }
    }


setDirectionalLight : Vec3 -> Options -> Options
setDirectionalLight x options =
    (\lights -> { options | lights = { lights | directional = x } }) options.lights


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
