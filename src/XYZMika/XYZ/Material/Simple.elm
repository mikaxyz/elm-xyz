module XYZMika.XYZ.Material.Simple exposing (material)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material


material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader :
    Shader Vertex
        { u
            | perspective : Mat4
            , camera : Mat4
            , worldMatrix : Mat4
        }
        { v_color : Vec3
        }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;

        varying vec3 v_color;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            v_color = color;
        }
    |]


fragmentShader :
    Shader {}
        { u
            | perspective : Mat4
            , camera : Mat4
            , worldMatrix : Mat4
        }
        { v_color : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;

        varying vec3 v_color;
        
        void main () {
            gl_FragColor =  vec4( v_color, 1.0);
        }
    |]
