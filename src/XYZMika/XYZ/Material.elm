module XYZMika.XYZ.Material exposing
    ( Renderer
    , fragmentShader
    , material
    , uniforms
    , vertexShader
    )

import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Object exposing (Object)


type alias Renderer materialId uniforms =
    Maybe materialId
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
