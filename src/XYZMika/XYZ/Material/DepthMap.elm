module XYZMika.XYZ.Material.DepthMap exposing
    ( fragmentShader
    , renderer
    , vertexShader
    )

import WebGL exposing (Entity, Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


renderer : Material.Options -> Uniforms u -> Object objectId materialId -> Entity
renderer _ uniforms object =
    material uniforms
        |> Material.toEntity object


material : Uniforms u -> Material (Uniforms u) {}
material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader : Shader Vertex (Uniforms u) {}
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} u {}
fragmentShader =
    [glsl|
        precision mediump float;

        void main () {
          const vec4 bitShift = vec4(1.0, 256.0, 256.0 * 256.0, 256.0 * 256.0 * 256.0);
          const vec4 bitMask = vec4(1.0/256.0, 1.0/256.0, 1.0/256.0, 0.0);
          vec4 rgbaDepth = fract(gl_FragCoord.z * bitShift); // Calculate the value stored into each byte
          rgbaDepth -= rgbaDepth.gbaa * bitMask; // Cut off the value which do not fit in 8 bits
          gl_FragColor = rgbaDepth;
        }
    |]
