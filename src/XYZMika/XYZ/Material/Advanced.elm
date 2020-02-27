module XYZMika.XYZ.Material.Advanced exposing (renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


directionalLight : Vec3
directionalLight =
    Vec3.fromRecord { x = 1, y = 0.7, z = 0.5 }


renderer : Texture -> Uniforms u -> Object materialId -> Entity
renderer defaultTexture uniforms object =
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
            , directionalLight = directionalLight
            , diffuseMap = object |> Object.diffuseMapWithDefault defaultTexture
            , hasDiffuseMap = Object.diffuseMap object /= Nothing
            , hasNormalMap = Object.normalMap object /= Nothing
            , normalMap = object |> Object.normalMapWithDefault defaultTexture
            , perspective = uniforms.perspective
            , worldMatrix = uniforms.worldMatrix
            , normalMapIntensity = object |> Object.normalMapIntensityWithDefault 2.0
            , uColor = Object.colorVec3 object
            }
        )


material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader :
    Shader
        { color : Vec3
        , meta : { hasColor : Bool, hasNormal : Bool, hasUV : Bool }
        , normal : Vec3
        , position : Vec3
        , uv : Vec2
        }
        { u
            | perspective : Mat4
            , camera : Mat4
            , worldMatrix : Mat4
            , uColor : Vec3

            --
            , directionalLight : Vec3

            --
            , diffuseMap : Texture
            , hasDiffuseMap : Bool
            , normalMap : Texture
            , hasNormalMap : Bool
            , normalMapIntensity : Float
        }
        { v_color : Vec3
        , v_normal : Vec3
        , v_uv : Vec2
        }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;
        attribute vec2 uv;
        
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;
        uniform vec3 uColor;

        uniform sampler2D diffuseMap;
        uniform bool hasDiffuseMap;

        uniform sampler2D normalMap;
        uniform bool hasNormalMap;
        uniform float normalMapIntensity;

        uniform vec3 directionalLight;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec2 v_uv;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            v_color = color * uColor;
            v_uv = uv;
            v_normal = normal;
        }
    |]


fragmentShader :
    Shader {}
        { u
            | perspective : Mat4
            , camera : Mat4
            , worldMatrix : Mat4
            , uColor : Vec3

            --
            , directionalLight : Vec3

            --
            , diffuseMap : Texture
            , hasDiffuseMap : Bool
            , normalMap : Texture
            , hasNormalMap : Bool
            , normalMapIntensity : Float
        }
        { v_color : Vec3
        , v_normal : Vec3
        , v_uv : Vec2
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D diffuseMap;
        uniform bool hasDiffuseMap;

        uniform sampler2D normalMap;
        uniform bool hasNormalMap;
        uniform float normalMapIntensity;

        uniform vec3 directionalLight;
        uniform mat4 worldMatrix;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec2 v_uv;

        void main () {
            vec3 diffuse;
            if(hasDiffuseMap) {
                diffuse = texture2D(diffuseMap, v_uv).rgb;
            } else {
                diffuse = vec3(1, 1, 1);
            }

            vec3 normal;
            if(hasNormalMap) {
                normal = texture2D(normalMap, v_uv).rgb;
                normal = v_normal * normalize(normal) * normalMapIntensity;
            } else {
                normal = v_normal;
            }

            // Lighting
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = worldMatrix * vec4(normal, 0.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            vec3 f_lighting = (directionalLightColor * directional);

            gl_FragColor =  vec4(v_color * diffuse * f_lighting, 1.0);
        }
    |]
