module ShadowMapping.Material.Advanced exposing (renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import ShadowMapping.Material.Textured
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
    , spotLight1 : Vec4
    , spotLight1direction : Vec3
    , spotLight1Color : Vec3

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
    , shadowMap : Texture
    , shadowMapCameraMatrix : Mat4
    , shadowMapPerspectiveMatrix : Mat4
    , shadowMapModelMatrix : Mat4
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_tangent : Vec3
    , v_uv : Vec2
    , v_fragPos : Vec3
    , v_Vertex_relative_to_light : Vec4

    --
    , v_t : Vec3
    , v_b : Vec3
    , v_n : Vec3
    }


type alias ShadowMap =
    { texture : Texture
    , perspectiveMatrix : Mat4
    , cameraMatrix : Mat4
    , modelMatrix : Mat4
    }


objectTextureMaps : Object materialId -> Maybe Texture
objectTextureMaps object =
    [ Object.diffuseMap object
    , Object.normalMap object
    ]
        |> List.filterMap identity
        |> List.head


renderer : ShadowMap -> Material.Options -> Scene.Uniforms u -> Object materialId -> Entity
renderer shadowMap options uniforms object =
    let
        spotLight : Int -> { light : Vec4, direction : Vec3, color : Vec3 }
        spotLight i =
            options
                |> Material.spotLightByIndex (i - 1)
                |> Maybe.map
                    (\light ->
                        { light = SpotLight.toVec4 light
                        , direction = SpotLight.direction light
                        , color = SpotLight.color light
                        }
                    )
                |> Maybe.withDefault
                    { light = vec4 0 0 0 0
                    , direction = vec3 0 0 0
                    , color = vec3 0 0 0
                    }

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
    case objectTextureMaps object of
        Just fallbackTexture ->
            ShadowMapping.Material.Textured.renderer
                shadowMap
                fallbackTexture
                options
                uniforms
                object

        Nothing ->
            material
                { sceneCamera = uniforms.sceneCamera
                , scenePerspective = uniforms.scenePerspective
                , sceneMatrix = uniforms.sceneMatrix
                , sceneRotationMatrix = uniforms.sceneRotationMatrix

                --
                , objectColor = Object.colorVec3 object
                , directionalLight = directionalLight

                --
                , spotLight1 = spotLight 1 |> .light
                , spotLight1Color = spotLight 1 |> .color
                , spotLight1direction = spotLight 1 |> .direction

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

                --
                , shadowMap = shadowMap.texture
                , shadowMapCameraMatrix = shadowMap.cameraMatrix
                , shadowMapPerspectiveMatrix = shadowMap.perspectiveMatrix
                , shadowMapModelMatrix = shadowMap.modelMatrix
                }
                |> Material.toEntityWithSettings
                    [ WebGL.Settings.DepthTest.default
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

        uniform mat4 shadowMapCameraMatrix;
        uniform mat4 shadowMapPerspectiveMatrix;
        uniform mat4 shadowMapModelMatrix;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_tangent;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        varying vec3 v_t;
        varying vec3 v_b;
        varying vec3 v_n;

        varying vec4 v_Vertex_relative_to_light;

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
            v_Vertex_relative_to_light = shadowMapPerspectiveMatrix * shadowMapCameraMatrix * pos;
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
        uniform vec3 spotLight1Color;
        uniform vec3 spotLight1direction;
        uniform sampler2D shadowMap;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec3 v_tangent;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        varying vec3 v_t;
        varying vec3 v_b;
        varying vec3 v_n;

        varying vec4 v_Vertex_relative_to_light;

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

        vec3 f_spotLight (vec3 normal, vec3 lightPosition, vec3 lightDirection) {
            highp float innerLimit = .99;
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
            vec3 diffuse = v_color;
            vec3 normal = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));

            vec3 lighting = vec3(0,0,0);
            if (spotLight1.w > 0.0) {
               lighting += spotLight1.w * f_spotLight(normal, spotLight1.xyz, spotLight1direction.xyz) * spotLight1Color;
            }

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
            float visibility = 1.0;
            const bool SHADOWS = true;
            const bool SOFT_SHADOWS = false;
            const float TEXEL_SIZE_MULTIPLIER = 1.0;

            if (SHADOWS == true) {
                vec3 shadowMapCoords = (v_Vertex_relative_to_light.xyz/v_Vertex_relative_to_light.w)/2.0 + 0.5;
                float shadowMapWidth = 640.0;
                float shadowMapHeight = 640.0;
                float bias = 0.0001;
                bool within = (shadowMapCoords.x >= -1.0) && (shadowMapCoords.x <= 1.0) && (shadowMapCoords.y >= -1.0) && (shadowMapCoords.y <= 1.0);

                if (within && SOFT_SHADOWS == true) {
                    vec2 texelSize = vec2(vec2(1.0/shadowMapWidth, 1.0/shadowMapHeight));
                    texelSize = texelSize * TEXEL_SIZE_MULTIPLIER;
                    visibility = getAverageVisibility(shadowMap, shadowMapCoords, texelSize, bias);
                } else if (within) {
                    visibility = getVisibility(shadowMap, shadowMapCoords.xy, shadowMapCoords.z, bias);
                }
            }

            // Clamp some...
//            visibility = 0.6 + (visibility * 0.4);
//            lighting = 0.2 + (lighting * 0.8);

            gl_FragColor =  vec4(visibility * lighting * diffuse, 1.0);
        }
    |]
