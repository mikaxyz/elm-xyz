module XYZMika.XYZ.Material.DepthMap exposing (fragmentShader, renderer, vertexShader)

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Entity, Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type alias Varyings =
    {}


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
        
        

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
//            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} u Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        
        
        
        
        void main () {
//            gl_FragColor = vec4(gl_FragCoord.w, gl_FragCoord.w, gl_FragCoord.w, 1.0);                
            
          const vec4 bitShift = vec4(1.0, 256.0, 256.0 * 256.0, 256.0 * 256.0 * 256.0);
          const vec4 bitMask = vec4(1.0/256.0, 1.0/256.0, 1.0/256.0, 0.0);
          vec4 rgbaDepth = fract(gl_FragCoord.z * bitShift); // Calculate the value stored into each byte
          rgbaDepth -= rgbaDepth.gbaa * bitMask; // Cut off the value which do not fit in 8 bits
          gl_FragColor = rgbaDepth;
        }
    |]
