module XYZMika.XYZ.Material.Advanced exposing (renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms as Scene



-- NORMAL MAPPING: https://learnopengl.com/Advanced-Lighting/Normal-Mapping


type alias Uniforms =
    { sceneCamera : Mat4
    , scenePerspective : Mat4
    , sceneMatrix : Mat4
    , sceneRotationMatrix : Mat4
    , objectColor : Vec3
    , directionalLight : Vec3
    , diffuseMap : Texture
    , hasDiffuseMap : Bool
    , normalMap : Texture
    , hasNormalMap : Bool

    --
    , pointLight1 : Vec4
    , pointLight2 : Vec4
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_tangent : Vec3
    , v_uv : Vec2
    , v_fragPos : Vec3

    --
    , v_t : Vec3
    , v_b : Vec3
    , v_n : Vec3
    }


renderer : Material.Options -> Texture -> Scene.Uniforms u -> Object materialId -> Entity
renderer options defaultTexture uniforms object =
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , sceneRotationMatrix = uniforms.sceneRotationMatrix

        --
        , objectColor = Object.colorVec3 object
        , directionalLight = options.lights.directional
        , diffuseMap = object |> Object.diffuseMapWithDefault defaultTexture
        , hasDiffuseMap = Object.diffuseMap object /= Nothing
        , hasNormalMap = Object.normalMap object /= Nothing
        , normalMap = object |> Object.normalMapWithDefault defaultTexture

        --
        , pointLight1 = Light.toVec4 options.lights.point1
        , pointLight2 = Light.toVec4 options.lights.point2
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
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;
        attribute vec3 tangent;
        attribute vec2 uv;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;
        uniform mat4 sceneRotationMatrix;
        
        uniform vec3 objectColor;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_tangent;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        varying vec3 v_t;
        varying vec3 v_b;
        varying vec3 v_n;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
            v_color = color * objectColor;
            v_uv = uv;
            v_normal = normal;
            v_t = normalize(vec3(sceneRotationMatrix * vec4(tangent, 1.0)));
            v_n = normalize(vec3(sceneRotationMatrix * vec4(normal, 1.0)));
            v_b = cross(v_n, v_t);
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;
        uniform mat4 sceneRotationMatrix;

        uniform vec3 directionalLight;
        uniform sampler2D diffuseMap;
        uniform bool hasDiffuseMap;
        uniform sampler2D normalMap;
        uniform bool hasNormalMap;

        uniform vec4 pointLight1;
        uniform vec4 pointLight2;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_tangent;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        varying vec3 v_t;
        varying vec3 v_b;
        varying vec3 v_n;


        highp mat4 transpose(in highp mat4 inMatrix) {
            highp vec4 i0 = inMatrix[0];
            highp vec4 i1 = inMatrix[1];
            highp vec4 i2 = inMatrix[2];
            highp vec4 i3 = inMatrix[3];

            highp mat4 outMatrix = mat4(
                         vec4(i0.x, i1.x, i2.x, i3.x),
                         vec4(i0.y, i1.y, i2.y, i3.y),
                         vec4(i0.z, i1.z, i2.z, i3.z),
                         vec4(i0.w, i1.w, i2.w, i3.w)
                         );

            return outMatrix;
        }

        vec3 f_pointLight_PassThrough (vec3 normal, vec3 lightPosition) {
//            highp vec3 color = vec3(0.6, 0.5, 0.2);
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp float falloff = 2.0;
            highp vec3 position = vec3(sceneMatrix * vec4(lightPosition, 1.0));

            highp float distance = distance(position, v_fragPos);
            highp float intensity = max(falloff - distance, 0.0);
            return color * intensity;
        }

        vec3 f_pointLight (vec3 normal, vec3 lightPosition) {
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 pointLightDirection = normalize(lightPosition - v_fragPos);
            highp float pointLightDiff = max(dot(normal, pointLightDirection), 0.0);
            highp float intensity = pow(pointLightDiff, 1.0);
            highp float distance = distance(lightPosition, v_fragPos);

//            return color * (intensity / distance);
            return color * intensity;
        }

        vec3 f_directionalLight (vec3 normal) {
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 directionalVector = normalize(directionalLight);
            highp float intensity = max(dot(normal, directionalVector), 0.0);
            return color * intensity;
        }

        void main () {
            vec3 diffuse;
            if(hasDiffuseMap) {
                diffuse = texture2D(diffuseMap, v_uv).rgb;
            } else {
                diffuse = v_color;
            }


            highp mat3 TBN = mat3(v_t, v_b, v_n);
            vec3 normal;
            if(hasNormalMap) {
                normal = texture2D(normalMap, v_uv).xyz;
                normal = normal * 2.0 - 1.0;
                normal = normalize(TBN * normal);
            } else {
                normal = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));
            }

//            vec3 lighting = vec3(0,0,0);
//            lighting += 0.3 * f_directionalLight(normal);
//            lighting += 0.3 * f_pointLight(normal);
//            lighting += 0.3 * f_pointLight_PassThrough(normal);


            vec3 lighting = vec3(0,0,0);
            if (pointLight1.w > 0.0) {
               lighting += pointLight1.w * f_pointLight(normal, pointLight1.xyz);
            }
            if (pointLight2.w > 0.0) {
               lighting += pointLight2.w * f_pointLight(normal, pointLight2.xyz);
            }
//            lighting += 0.8 * f_pointLight_PassThrough(normal);

//            vec3 lighting = vec3(1,1,1);
////            lighting *= f_directionalLight(normal);
//            lighting *= f_pointLight(normal);
////            lighting *= f_pointLight_PassThrough(normal);

            gl_FragColor =  vec4(lighting * diffuse, 1.0);
        }
    |]
