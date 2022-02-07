module XYZMika.XYZ.Material.DepthMap exposing (fragmentShader, renderer, vertexShader)

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Entity, Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type alias Varyings =
    { v_fragPos : Vec3 }


renderer : Material.Options -> Uniforms u -> Object materialId -> Entity
renderer _ uniforms object =
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
        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;
        
        varying vec3 v_fragPos;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} u Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        
        varying vec3 v_fragPos;
        
        void main () {
//            gl_FragColor =  vec4(gl_FragCoord.z, 1.0);
//            gl_FragColor = gl_FragCoord;
//            gl_FragColor = vec4(v_fragPos, 1.0);
            gl_FragColor = vec4(gl_FragCoord.w, gl_FragCoord.w, gl_FragCoord.w, 1.0);
            
        }
    |]
