module XYZMika.XYZ.Material.Color exposing (renderer)

import Math.Matrix4 exposing (Mat4)
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
        (material
            -- TODO: Alphabetize these
            -- { aCamera, aWorldMatrix, aPerspective, texDiffuse, texHasDiffuse, etc }
            { camera = uniforms.camera
            , perspective = uniforms.perspective
            , worldMatrix = uniforms.worldMatrix
            , uColor = Object.colorVec3 object
            }
        )


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
            , uColor : Vec3
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
        uniform vec3 uColor;

        varying vec3 v_color;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            v_color = color * uColor;
        }
    |]


fragmentShader : Shader {} (Uniforms u) { v_color : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;

        varying vec3 v_color;
        
        void main () {
            gl_FragColor =  vec4(v_color, 1.0);
        }
    |]
