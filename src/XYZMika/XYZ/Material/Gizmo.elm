module XYZMika.XYZ.Material.Gizmo exposing (fragmentShader, renderer, vertexShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms as Scene


type alias Uniforms =
    { sceneCamera : Mat4
    , scenePerspective : Mat4
    , sceneMatrix : Mat4
    , sceneRotationMatrix : Mat4

    --
    , center : Vec3
    }


type alias Varyings =
    { v_fragPos : Vec3
    }


renderer : Material.Options -> Texture -> Scene.Uniforms u -> Object materialId -> Entity
renderer _ _ uniforms object =
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , sceneRotationMatrix = uniforms.sceneRotationMatrix
        , center = Object.position object
        }
        |> Material.toEntity object


material : Uniforms -> Material Uniforms Varyings
material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision highp float;

        attribute vec3 position;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;

        uniform vec3 center;
        varying vec3 v_fragPos;

        void main () {
            vec3 pos = center + position.xyz;
            vec4 pos2 = sceneCamera * sceneMatrix * (vec4(center, 1.0));
            pos2.x += position.x;
            pos2.y += position.y;
            gl_Position = scenePerspective * pos2;
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision highp float;

        varying vec3 v_fragPos;
        
        void main () {
            float x = v_fragPos.x;
            float y = v_fragPos.y;

            // Circle
            float r1, r2;
            float radius = 0.4;
            float thickness = 0.05;
            r1 = step(length(v_fragPos.xy), radius);
            r2 = step(length(v_fragPos.xy), radius - thickness);
            float circle = r1 - r2;

            // Rays
            float lineWidth = 0.05;
            lineWidth *= 0.5;
            float lineWidthDiag = 1.333 * lineWidth;
            float lMask = step(length(v_fragPos.xy), 1.0) - step(length(v_fragPos.xy), 0.7);
            float l1 = x < (lineWidth) && x > -(lineWidth) ? 1.0 : 0.0;
            float l2 = y < (lineWidth) && y > -(lineWidth) ? 1.0 : 0.0;
            float l3 = x > (y - lineWidthDiag) && x < (y + lineWidthDiag) ? 1.0 : 0.0;
            float l4 = -x > (y - lineWidthDiag) && -x < (y + lineWidthDiag) ? 1.0 : 0.0;
            float rays = (l1 + l2 + l3 + l4) * lMask;

            float draw = circle + rays;

            gl_FragColor = vec4(1.0, 1.0, 1.0, draw);
        }
    |]
