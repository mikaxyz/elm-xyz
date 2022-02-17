module XYZMika.XYZ.Material.Advanced.TexturedWithShadowMaps exposing (renderer)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material, ShadowMaps)
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
    , spotLight3 : Vec4
    , spotLight3_direction : Vec3
    , spotLight3_color : Vec3
    , spotLight3_fov : Float
    , spotLight3_resolution : Float
    , spotLight3_shadowMap : Texture
    , spotLight3_shadowMapViewMatrix : Mat4

    --
    , spotLight4 : Vec4
    , spotLight4_direction : Vec3
    , spotLight4_color : Vec3
    , spotLight4_fov : Float
    , spotLight4_resolution : Float
    , spotLight4_shadowMap : Texture
    , spotLight4_shadowMapViewMatrix : Mat4

    --
    , spotLight5 : Vec4
    , spotLight5_direction : Vec3
    , spotLight5_color : Vec3
    , spotLight5_fov : Float
    , spotLight5_resolution : Float
    , spotLight5_shadowMap : Texture
    , spotLight5_shadowMapViewMatrix : Mat4

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
    , v_vertexRelativeToSpotLight3 : Vec4
    , v_vertexRelativeToSpotLight4 : Vec4
    , v_vertexRelativeToSpotLight5 : Vec4

    --
    , v_t : Vec3
    , v_b : Vec3
    , v_n : Vec3
    }


renderer : Texture -> ShadowMaps -> Material.Options -> Scene.Uniforms u -> Object materialId -> Entity
renderer fallbackTexture shadowMaps options uniforms object =
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
        , spotLight1_shadowMap =
            shadowMaps.shadowMap1
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.fallbackTexture
        , spotLight1_shadowMapViewMatrix =
            shadowMaps.shadowMap1
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault Mat4.identity

        --
        , spotLight2 = spotLight 2 |> .light
        , spotLight2_color = spotLight 2 |> .color
        , spotLight2_direction = spotLight 2 |> .direction
        , spotLight2_fov = spotLight 2 |> .fov
        , spotLight2_resolution = spotLight 2 |> .resolution
        , spotLight2_shadowMap =
            shadowMaps.shadowMap2
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.fallbackTexture
        , spotLight2_shadowMapViewMatrix =
            shadowMaps.shadowMap2
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault Mat4.identity

        --
        , spotLight3 = spotLight 3 |> .light
        , spotLight3_color = spotLight 3 |> .color
        , spotLight3_direction = spotLight 3 |> .direction
        , spotLight3_fov = spotLight 3 |> .fov
        , spotLight3_resolution = spotLight 3 |> .resolution
        , spotLight3_shadowMap =
            shadowMaps.shadowMap3
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.fallbackTexture
        , spotLight3_shadowMapViewMatrix =
            shadowMaps.shadowMap3
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault Mat4.identity

        --
        , spotLight4 = spotLight 4 |> .light
        , spotLight4_color = spotLight 4 |> .color
        , spotLight4_direction = spotLight 4 |> .direction
        , spotLight4_fov = spotLight 4 |> .fov
        , spotLight4_resolution = spotLight 4 |> .resolution
        , spotLight4_shadowMap =
            shadowMaps.shadowMap4
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.fallbackTexture
        , spotLight4_shadowMapViewMatrix =
            shadowMaps.shadowMap4
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault Mat4.identity

        --
        , spotLight5 = spotLight 5 |> .light
        , spotLight5_color = spotLight 5 |> .color
        , spotLight5_direction = spotLight 5 |> .direction
        , spotLight5_fov = spotLight 5 |> .fov
        , spotLight5_resolution = spotLight 5 |> .resolution
        , spotLight5_shadowMap =
            shadowMaps.shadowMap5
                |> Maybe.map .texture
                |> Maybe.withDefault shadowMaps.fallbackTexture
        , spotLight5_shadowMapViewMatrix =
            shadowMaps.shadowMap5
                |> Maybe.map .viewMatrix
                |> Maybe.withDefault Mat4.identity

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


        uniform mat4 spotLight1_shadowMapViewMatrix;
        uniform mat4 spotLight2_shadowMapViewMatrix;
        uniform mat4 spotLight3_shadowMapViewMatrix;
        uniform mat4 spotLight4_shadowMapViewMatrix;
        uniform mat4 spotLight5_shadowMapViewMatrix;

        varying vec4 v_vertexRelativeToSpotLight1;
        varying vec4 v_vertexRelativeToSpotLight2;
        varying vec4 v_vertexRelativeToSpotLight3;
        varying vec4 v_vertexRelativeToSpotLight4;
        varying vec4 v_vertexRelativeToSpotLight5;

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
            v_vertexRelativeToSpotLight3 = spotLight3_shadowMapViewMatrix * pos;
            v_vertexRelativeToSpotLight4 = spotLight4_shadowMapViewMatrix * pos;
            v_vertexRelativeToSpotLight5 = spotLight5_shadowMapViewMatrix * pos;
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

        uniform vec4 spotLight3;
        uniform vec3 spotLight3_color;
        uniform vec3 spotLight3_direction;
        uniform float spotLight3_fov;
        uniform float spotLight3_resolution;
        uniform sampler2D spotLight3_shadowMap;
        varying vec4 v_vertexRelativeToSpotLight3;

        uniform vec4 spotLight4;
        uniform vec3 spotLight4_color;
        uniform vec3 spotLight4_direction;
        uniform float spotLight4_fov;
        uniform float spotLight4_resolution;
        uniform sampler2D spotLight4_shadowMap;
        varying vec4 v_vertexRelativeToSpotLight4;

        uniform vec4 spotLight5;
        uniform vec3 spotLight5_color;
        uniform vec3 spotLight5_direction;
        uniform float spotLight5_fov;
        uniform float spotLight5_resolution;
        uniform sampler2D spotLight5_shadowMap;
        varying vec4 v_vertexRelativeToSpotLight5;

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

        // Spotlights/Shadow mapping
        highp float getDepth
        ( sampler2D shadowMapTexture
        , vec2 shadowMapCoords
        ) {
            vec4 rgbaDepth = texture2D(shadowMapTexture, shadowMapCoords);
            vec4 bitShift = vec4(1.0, 1.0/256.0, 1.0/(256.0*256.0), 1.0/(256.0*256.0*256.0));
            return dot(rgbaDepth, bitShift);
        }

        highp float getVisibility
        ( sampler2D shadowMapTexture
        , vec2 shadowMapCoords
        , float realDepth
        , float acneBias
        ) {
            float depth = getDepth(shadowMapTexture, shadowMapCoords.xy);
            float visibility = (realDepth > depth + acneBias) ? 0.0 : 1.0;
            return visibility;
        }

        highp float getAverageVisibility
        ( sampler2D shadowMapTexture
        , vec3 shadowMapCoords
        , vec2 texelSize
        , float acneBias
        ) {
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

        highp float applyShadowMap
        ( sampler2D shadowMap
        , vec4 vertexRelativeToSpotLight
        , float resolution
        ) {
            const bool SOFT_SHADOWS = true;
            const float TEXEL_SIZE_MULTIPLIER = 1.0;

            float visibility = 1.0;
            vec3 lighting = vec3(0.0, 0.0, 0.0);

            vec3 shadowMapCoords = (vertexRelativeToSpotLight.xyz/vertexRelativeToSpotLight.w)/2.0 + 0.5;
            float shadowMapWidth = resolution;
            float shadowMapHeight = resolution;
            float bias = 0.0001;

            if (SOFT_SHADOWS) {
                vec2 texelSize = vec2(vec2(1.0/shadowMapWidth, 1.0/shadowMapHeight));
                texelSize = texelSize * TEXEL_SIZE_MULTIPLIER;
                visibility = getAverageVisibility(shadowMap, shadowMapCoords, texelSize, bias);
            } else {
                visibility = getVisibility(shadowMap, shadowMapCoords.xy, shadowMapCoords.z, bias);
            }

            return visibility;
        }

        vec3 applySpotLight
        ( vec3 normal
        , vec3 lightPosition
        , vec3 lightDirection
        , vec3 color
        , float fov

        , sampler2D shadowMap
        , vec4 vertexRelativeToSpotLight
        , float resolution
        ) {
            highp float innerLimit = fov;
            highp float outerLimit = innerLimit - 0.02;
            float shininess = 1.0;
            highp vec3 direction = normalize(lightPosition - v_fragPos);

            highp float dotFromDirection = dot(direction, -lightDirection);
            float inLight = smoothstep(outerLimit, innerLimit, dotFromDirection);
            float light = inLight * dot(normal, direction);
            float specular = inLight * pow(dot(normal, direction), shininess);

            float shadow = applyShadowMap
                ( shadowMap
                , vertexRelativeToSpotLight
                , resolution
                );

            return color * light * specular * shadow;
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

            // Spotlights
            lighting += applySpotLight
                ( normal
                , spotLight1.xyz
                , spotLight1_direction.xyz
                , spotLight1_color
                , spotLight1_fov
                , spotLight1_shadowMap
                , v_vertexRelativeToSpotLight1
                , spotLight1_resolution
                );

            lighting += applySpotLight
                ( normal
                , spotLight2.xyz
                , spotLight2_direction.xyz
                , spotLight2_color
                , spotLight2_fov
                , spotLight2_shadowMap
                , v_vertexRelativeToSpotLight2
                , spotLight2_resolution
                );

            lighting += applySpotLight
                ( normal
                , spotLight3.xyz
                , spotLight3_direction.xyz
                , spotLight3_color
                , spotLight3_fov
                , spotLight3_shadowMap
                , v_vertexRelativeToSpotLight3
                , spotLight3_resolution
                );

            lighting += applySpotLight
                ( normal
                , spotLight4.xyz
                , spotLight4_direction.xyz
                , spotLight4_color
                , spotLight4_fov
                , spotLight4_shadowMap
                , v_vertexRelativeToSpotLight4
                , spotLight4_resolution
                );

            lighting += applySpotLight
                ( normal
                , spotLight5.xyz
                , spotLight5_direction.xyz
                , spotLight5_color
                , spotLight5_fov
                , spotLight5_shadowMap
                , v_vertexRelativeToSpotLight5
                , spotLight5_resolution
                );

            gl_FragColor =  vec4(lighting * diffuse, 1.0);
        }
    |]
