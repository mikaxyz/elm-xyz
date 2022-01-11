module XYZMika.XYZ.Material.Grid exposing (fragmentShader, renderer, vertexShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
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
    , color : Vec3
    , axis : Int
    }


type alias Varyings =
    { v_fragPos : Vec3
    , v_uv : Vec3
    }


renderer : Material.Options -> Texture -> Scene.Uniforms u -> Object materialId -> Entity
renderer _ _ uniforms object =
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , sceneRotationMatrix = uniforms.sceneRotationMatrix

        --
        , color = Object.colorVec3 object
        , axis = 1
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
        attribute vec3 color;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;

        varying vec3 v_fragPos;
        varying vec3 v_uv;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
            v_uv = position;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision highp float;

        uniform int axis;
        varying vec3 v_fragPos;
        varying vec3 v_uv;
        
        void main () {
            float x = v_fragPos.x;
            float y = v_fragPos.y;


            vec3 xColor = vec3(1, 0, 0);
            vec3 yColor = vec3(0, 1, 0);

            if (axis == 0) {
                x = v_fragPos.z;
                y = v_fragPos.y;
                xColor = vec3(0, 0, 1);
            } else if (axis == 1) {
                x = v_fragPos.z;
                y = v_fragPos.x;
                xColor = vec3(0, 0, 1);
                yColor = vec3(1, 0, 0);
//            } else if (axis == 2) {
//                x = v_fragPos.x;
//                y = v_fragPos.y;
            }

            vec2 coord = vec2(x, y);

            // Compute anti-aliased world-space grid lines
            vec2 grid = abs(fract(coord - 0.5) - 0.5) / fwidth(coord);
            float line = min(grid.x, grid.y);

            // Dots:
            // line = max(grid.x, grid.y);

            float opacity = 1.0;
            float distance = distance(vec2(0.0), coord);
            opacity = 1.0 / distance;
//            i = min(10.0 - (distance), 1.0);
//            i = 3.0 - v_fragPos.x;

            // Cool
//            float i = min(10.0 - (distance), 1.0);
//            gl_FragColor = vec4(color, i - min(line, i) * i);

            vec3 col =  v_fragPos * 0.5 + 0.5;

            if (x < 0.1 && x > -0.1) {
                col = yColor;
                opacity = 1.0;
            }
            if (y < 0.1 && y > -0.1) {
                col = xColor;
                opacity = 1.0;
            }

            // Cool
            // gl_FragColor = vec4(col, opacity - min(line, 1.0) * (opacity) * 0.2);
            gl_FragColor = vec4(col, opacity - min(line, 1.0) * opacity);
        }
    |]
