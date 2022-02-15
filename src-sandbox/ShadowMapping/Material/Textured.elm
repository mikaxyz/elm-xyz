module ShadowMapping.Material.Textured exposing (renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight
import XYZMika.XYZ.Scene.Light.PointLight as PointLight
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms as Scene


type alias Uniforms =
    { sceneCamera : Mat4
    , scenePerspective : Mat4
    , sceneMatrix : Mat4
    , sceneRotationMatrix : Mat4
    , objectColor : Vec3
    , directionalLight : Vec4

    --
    , diffuseMap : Texture
    , hasDiffuseMap : Bool
    , normalMap : Texture
    , hasNormalMap : Bool

    --
    , spotLight1 : Vec4
    , spotLight1_direction : Vec3
    , spotLight1_color : Vec3
    , spotLight1_fov : Float
    , spotLight1_resolution : Float
    , spotLight1_shadowMap : Texture
    , spotLight1_shadowMapViewMatrix : Mat4

    --
    , spotLight2 : Vec4
    , spotLight2_direction : Vec3
    , spotLight2_color : Vec3
    , spotLight2_fov : Float
    , spotLight2_resolution : Float
    , spotLight2_shadowMap : Texture
    , spotLight2_shadowMapViewMatrix : Mat4

    --
    , pointLight1 : Vec4
    , pointLight1Color : Vec3
    , pointLight2 : Vec4
    , pointLight2Color : Vec3
    , pointLight3 : Vec4
    , pointLight3Color : Vec3
    , pointLight4 : Vec4
    , pointLight4Color : Vec3
    , pointLight5 : Vec4
    , pointLight5Color : Vec3
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_tangent : Vec3
    , v_uv : Vec2
    , v_fragPos : Vec3
    , v_vertexRelativeToSpotLight1 : Vec4
    , v_vertexRelativeToSpotLight2 : Vec4

    --
    , v_t : Vec3
    , v_b : Vec3
    , v_n : Vec3
    }


type alias ShadowMap =
    { texture : Texture
    , viewMatrix : Mat4
    }


type alias ShadowMaps =
    { shadowMap1 : ShadowMap
    , shadowMap2 : Maybe ShadowMap
    }


renderer : ShadowMaps -> Texture -> Material.Options -> Scene.Uniforms u -> Object materialId -> Entity
renderer shadowMaps fallbackTexture options uniforms object =
    let
        spotLight : Int -> SpotLight.ShaderData
        spotLight i =
            options
                |> Material.spotLightByIndex (i - 1)
                |> SpotLight.toShaderData

        pointLight : Int -> { light : Vec4, color : Vec3 }
        pointLight i =
            options
                |> Material.pointLightByIndex (i - 1)
                |> Maybe.map
                    (\light ->
                        { light = PointLight.toVec4 light
                        , color = PointLight.color light
                        }
                    )
                |> Maybe.withDefault
                    { light = vec4 0 0 0 0
                    , color = vec3 0 0 0
                    }

        directionalLight =
            options
                |> Material.directionalLights
                |> List.head
                |> Maybe.map DirectionalLight.toVec4
                |> Maybe.withDefault (vec4 0 0 0 0)
    in
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , sceneRotationMatrix = uniforms.sceneRotationMatrix

        --
        , objectColor = Object.colorVec3 object
        , directionalLight = directionalLight

        --
        , diffuseMap = object |> Object.diffuseMapWithDefault fallbackTexture
        , hasDiffuseMap = Object.diffuseMap object /= Nothing
        , hasNormalMap = Object.normalMap object /= Nothing
        , normalMap = object |> Object.normalMapWithDefault fallbackTexture

        --
        , spotLight1 = spotLight 1 |> .light
        , spotLight1_color = spotLight 1 |> .color
        , spotLight1_direction = spotLight 1 |> .direction
        , spotLight1_fov = spotLight 1 |> .fov
        , spotLight1_resolution = spotLight 1 |> .resolution
        , spotLight1_shadowMap = shadowMaps.shadowMap1.texture
        , spotLight1_shadowMapViewMatrix = shadowMaps.shadowMap1.viewMatrix

        --
        , spotLight2 = spotLight 2 |> .light
        , spotLight2_color = spotLight 2 |> .color
        , spotLight2_direction = spotLight 2 |> .direction
        , spotLight2_fov = spotLight 2 |> .fov
        , spotLight2_resolution = spotLight 2 |> .resolution
        , spotLight2_shadowMap =
            shadowMaps.shadowMap2
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.shadowMap1.texture
        , spotLight2_shadowMapViewMatrix =
            shadowMaps.shadowMap2
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault shadowMaps.shadowMap1.viewMatrix

        --
        , pointLight1 = pointLight 1 |> .light
        , pointLight1Color = pointLight 1 |> .color
        , pointLight2 = pointLight 2 |> .light
        , pointLight2Color = pointLight 2 |> .color
        , pointLight3 = pointLight 3 |> .light
        , pointLight3Color = pointLight 3 |> .color
        , pointLight4 = pointLight 4 |> .light
        , pointLight4Color = pointLight 4 |> .color
        , pointLight5 = pointLight 5 |> .light
        , pointLight5Color = pointLight 5 |> .color
        }
        |> Material.toEntityWithSettings
            [ WebGL.Settings.DepthTest.default

            --, WebGL.Settings.cullFace WebGL.Settings.back
            ]
            object


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


        uniform mat4 spotLight1_shadowMapViewMatrix;
        uniform mat4 spotLight2_shadowMapViewMatrix;

        varying vec4 v_vertexRelativeToSpotLight1;
        varying vec4 v_vertexRelativeToSpotLight2;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
            v_color = color * objectColor;
            v_uv = uv;
            v_normal = normal;
            v_t = normalize(vec3(sceneRotationMatrix * vec4(tangent, 1.0)));
            v_n = normalize(vec3(sceneRotationMatrix * vec4(normal, 1.0)));
            v_b = cross(v_n, v_t);

            vec4 pos = vec4(v_fragPos, 1.0);
            pos = sceneMatrix * vec4(position, 1.0);
            v_vertexRelativeToSpotLight1 = spotLight1_shadowMapViewMatrix * pos;
            v_vertexRelativeToSpotLight2 = spotLight2_shadowMapViewMatrix * pos;
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

        uniform vec4 directionalLight;
        uniform sampler2D diffuseMap;
        uniform bool hasDiffuseMap;
        uniform sampler2D normalMap;
        uniform bool hasNormalMap;

        uniform vec4 pointLight1;
        uniform vec3 pointLight1Color;
        uniform vec4 pointLight2;
        uniform vec3 pointLight2Color;
        uniform vec4 pointLight3;
        uniform vec3 pointLight3Color;
        uniform vec4 pointLight4;
        uniform vec3 pointLight4Color;
        uniform vec4 pointLight5;
        uniform vec3 pointLight5Color;

        uniform vec4 spotLight1;
        uniform vec3 spotLight1_color;
        uniform vec3 spotLight1_direction;
        uniform float spotLight1_fov;
        uniform float spotLight1_resolution;
        uniform sampler2D spotLight1_shadowMap;
        varying vec4 v_vertexRelativeToSpotLight1;

        uniform vec4 spotLight2;
        uniform vec3 spotLight2_color;
        uniform vec3 spotLight2_direction;
        uniform float spotLight2_fov;
        uniform float spotLight2_resolution;
        uniform sampler2D spotLight2_shadowMap;
        varying vec4 v_vertexRelativeToSpotLight2;

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

        vec3 f_spotLight (vec3 normal, vec3 lightPosition, vec3 lightDirection, float fov) {
            highp float innerLimit = fov;
            highp float outerLimit = innerLimit - 0.02;

            float shininess = 1.0;

            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 direction = normalize(lightPosition - v_fragPos);

            highp float dotFromDirection = dot(direction, -lightDirection);
            float inLight = smoothstep(outerLimit, innerLimit, dotFromDirection);
            float light = inLight * dot(normal, direction);
            float specular = inLight * pow(dot(normal, direction), shininess);

            return color * light * specular;
        }

        vec3 f_pointLight_PassThrough (vec3 normal, vec3 lightPosition) {
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

            return color * intensity;
        }

        vec3 f_directionalLight (vec3 normal) {
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 directionalVector = normalize(directionalLight.xyz);
            highp float intensity = max(dot(normal, directionalVector), 0.0);
            return color * intensity * directionalLight.w;
        }

        // Shadow mapping
        highp float getDepth(sampler2D shadowMapTexture, vec2 shadowMapCoords) {
            vec4 rgbaDepth = texture2D(shadowMapTexture, shadowMapCoords);
            vec4 bitShift = vec4(1.0, 1.0/256.0, 1.0/(256.0*256.0), 1.0/(256.0*256.0*256.0));
            return dot(rgbaDepth, bitShift);
        }

        highp float getVisibility(sampler2D shadowMapTexture, vec2 shadowMapCoords, float realDepth, float acneBias) {
            float depth = getDepth(shadowMapTexture, shadowMapCoords.xy);
            float visibility = (realDepth > depth + acneBias) ? 0.0 : 1.0;
            return visibility;
        }

        highp float getAverageVisibility(sampler2D shadowMapTexture, vec3 shadowMapCoords, vec2 texelSize, float acneBias) {
            vec3 coords;
            highp float depth;
            const int sampleSize = 2; // TODO:How to pass in this as uniform? Needs a constant...
            highp float visibility = 0.0;

            for (int x = -sampleSize; x <= sampleSize; x++) {
                for (int y = -sampleSize; y <= sampleSize; y++) {
                    vec2 samplePoint = shadowMapCoords.xy + (vec2(x, y) * texelSize);
                    visibility += getVisibility(shadowMapTexture, samplePoint, shadowMapCoords.z, acneBias);
                }
            }
            return visibility / float((sampleSize * 2 + 1) * (sampleSize * 2 + 1));
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

            vec3 lighting = vec3(0,0,0);
            if (pointLight1.w > 0.0) {
               lighting += pointLight1.w * f_pointLight(normal, pointLight1.xyz) * pointLight1Color;
            }
            if (pointLight2.w > 0.0) {
               lighting += pointLight2.w * f_pointLight(normal, pointLight2.xyz) * pointLight2Color;
            }
            if (pointLight3.w > 0.0) {
               lighting += pointLight3.w * f_pointLight(normal, pointLight3.xyz) * pointLight3Color;
            }
            if (pointLight4.w > 0.0) {
               lighting += pointLight4.w * f_pointLight(normal, pointLight4.xyz) * pointLight4Color;
            }
            if (pointLight5.w > 0.0) {
               lighting += pointLight5.w * f_pointLight(normal, pointLight5.xyz) * pointLight5Color;
            }


            if (directionalLight.w > 0.0) {
                lighting += f_directionalLight(normal);
            }

            // Shadows
            const bool SHADOWS = true;
            const bool SOFT_SHADOWS = true;
            const float TEXEL_SIZE_MULTIPLIER = 1.0;

            float visibility1 = 1.0;
            vec3 lighting1 = vec3(0.0, 0.0, 0.0);
            if (spotLight1.w > 0.0) {
               lighting1 += spotLight1.w * f_spotLight(normal, spotLight1.xyz, spotLight1_direction.xyz, spotLight1_fov) * spotLight1_color;
            }

            if (SHADOWS == true) {
                vec3 shadowMapCoords = (v_vertexRelativeToSpotLight1.xyz/v_vertexRelativeToSpotLight1.w)/2.0 + 0.5;
                float shadowMapWidth = spotLight1_resolution;
                float shadowMapHeight = spotLight1_resolution;
                float bias = 0.0001;
                bool within = true;
                // bool within = (shadowMapCoords.x >= -1.0) && (shadowMapCoords.x <= 1.0) && (shadowMapCoords.y >= -1.0) && (shadowMapCoords.y <= 1.0);

                if (within && SOFT_SHADOWS == true) {
                    vec2 texelSize = vec2(vec2(1.0/shadowMapWidth, 1.0/shadowMapHeight));
                    texelSize = texelSize * TEXEL_SIZE_MULTIPLIER;
                    visibility1 = getAverageVisibility(spotLight1_shadowMap, shadowMapCoords, texelSize, bias);
                } else if (within) {
                    visibility1 = getVisibility(spotLight1_shadowMap, shadowMapCoords.xy, shadowMapCoords.z, bias);
                }
            }

            float visibility2 = 1.0;
            vec3 lighting2 = vec3(0.0, 0.0, 0.0);
            if (spotLight2.w > 0.0) {
               lighting2 += spotLight2.w * f_spotLight(normal, spotLight2.xyz, spotLight2_direction.xyz, spotLight2_fov) * spotLight2_color;
            }

            if (SHADOWS == true && spotLight2_fov > 0.001) {
                vec3 shadowMapCoords = (v_vertexRelativeToSpotLight2.xyz/v_vertexRelativeToSpotLight2.w)/2.0 + 0.5;
                float shadowMapWidth = spotLight2_resolution;
                float shadowMapHeight = spotLight2_resolution;
                float bias = 0.0001;
                bool within = true;
                // bool within = (shadowMapCoords.x >= -1.0) && (shadowMapCoords.x <= 1.0) && (shadowMapCoords.y >= -1.0) && (shadowMapCoords.y <= 1.0);

                if (within && SOFT_SHADOWS == true) {
                    vec2 texelSize = vec2(vec2(1.0/shadowMapWidth, 1.0/shadowMapHeight));
                    texelSize = texelSize * TEXEL_SIZE_MULTIPLIER;
                    visibility2 = getAverageVisibility(spotLight2_shadowMap, shadowMapCoords, texelSize, bias);
                } else if (within) {
                    visibility2 = getVisibility(spotLight2_shadowMap, shadowMapCoords.xy, shadowMapCoords.z, bias);
                }
            }

            lighting += (lighting1 * visibility1);
            lighting += (lighting2 * visibility2);

            gl_FragColor =  vec4(lighting * diffuse, 1.0);
        }
    |]
