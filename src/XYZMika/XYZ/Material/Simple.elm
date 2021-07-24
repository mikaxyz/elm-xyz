module XYZMika.XYZ.Material.Simple exposing (fragmentShader, renderer, vertexShader)

import Math.Vector3 exposing (Vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type alias Varyings =
    { v_color : Vec3
    }


renderer : Material.Options -> Texture -> Uniforms u -> Object materialId -> Entity
renderer _ _ uniforms object =
    material uniforms
        |> Material.toEntity object


material : Uniforms u -> Material (Uniforms u) Varyings
material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader : Shader Vertex (Uniforms u) Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;

        varying vec3 v_color;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
            v_color = color;
        }
    |]


fragmentShader : Shader {} u Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 v_color;
        
        void main () {
            gl_FragColor =  vec4(v_color, 1.0);
        }
    |]
