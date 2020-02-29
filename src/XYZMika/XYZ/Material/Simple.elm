module XYZMika.XYZ.Material.Simple exposing (fragmentShader, renderer, vertexShader)

import Math.Vector3 exposing (Vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


renderer : Texture -> Uniforms u -> Object materialId -> Entity
renderer _ uniforms object =
    (\m ->
        WebGL.entity
            (Material.vertexShader m)
            (Material.fragmentShader m)
            (Object.mesh object)
            (Material.uniforms m)
    )
        (material uniforms)


material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader :
    Shader Vertex
        (Uniforms u)
        { v_color : Vec3
        }
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


fragmentShader : Shader {} (Uniforms u) { v_color : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 v_color;
        
        void main () {
            gl_FragColor =  vec4(v_color, 1.0);
        }
    |]
