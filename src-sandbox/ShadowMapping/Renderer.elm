module ShadowMapping.Renderer exposing (renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight
import XYZMika.XYZ.Scene.Light.PointLight as PointLight
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


renderer : ShadowMap -> Material.Options -> Scene.Uniforms u -> Object materialId -> Entity
renderer shadowMap options uniforms object =
    let
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
        , shadowMap = shadowMap.texture
        , shadowMapCameraMatrix = shadowMap.cameraMatrix
        , shadowMapPerspectiveMatrix = shadowMap.perspectiveMatrix
        , shadowMapModelMatrix = shadowMap.modelMatrix
        }
        |> Material.toEntityWithSettings
            [ WebGL.Settings.DepthTest.default

            --, WebGL.Settings.cullFace WebGL.Settings.back
            --, WebGL.Settings.DepthTest.default
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
        
        const mat4 texUnitConverter = mat4(0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.5, 0.5, 0.5, 1.0);

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
            v_Vertex_relative_to_light = shadowMapPerspectiveMatrix * shadowMapCameraMatrix  * pos;
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
        
        float shadowCalculation(vec4 fragPosLightSpace, sampler2D u_shadowMap, float bias) {
           // perform perspective divide and map to [0,1] range
           vec3 projCoords = fragPosLightSpace.xyz/fragPosLightSpace.w;
           projCoords = projCoords * 0.5 + 0.5;
           float shadowDepth = texture2D(u_shadowMap, projCoords.xy).r;
           float depth = projCoords.z;
           float shadow = step(depth-bias,shadowDepth);
           return shadow;
        }
        
        // Determine if this fragment is in a shadow. Returns true or false.
        bool in_shadow(void) {
            
          float u_Tolerance_constant = 0.2;
        
          // The vertex location rendered from the light source is almost in Normalized
          // Device Coordinates (NDC), but the perspective division has not been
          // performed yet. Perform the perspective divide. The (x,y,z) vertex location
          // components are now each in the range [-1.0,+1.0].
          vec3 vertex_relative_to_light = v_Vertex_relative_to_light.xyz / v_Vertex_relative_to_light.w;
        
          // Convert the the values from Normalized Device Coordinates (range [-1.0,+1.0])
          // to the range [0.0,1.0]. This mapping is done by scaling
          // the values by 0.5, which gives values in the range [-0.5,+0.5] and then
          // shifting the values by +0.5.
          vertex_relative_to_light = vertex_relative_to_light * 0.5 + 0.5;
        
          // Get the z value of this fragment in relationship to the light source.
          // This value was stored in the shadow map (depth buffer of the frame buffer)
          // which was passed to the shader as a texture map.
          vec4 shadowmap_color = texture2D(shadowMap, vertex_relative_to_light.xy);
        
          // The texture map contains a single depth value for each pixel. However,
          // the texture2D sampler always returns a color from a texture. For a
          // gl.DEPTH_COMPONENT texture, the color contains the depth value in
          // each of the color components. If the value was d, then the color returned
          // is (d,d,d,1). This is a "color" (depth) value between [0.0,+1.0].
          float shadowmap_distance = shadowmap_color.r;
        
          // Test the distance between this fragment and the light source as
          // calculated using the shadowmap transformation (vertex_relative_to_light.z) and
          // the smallest distance between the closest fragment to the light source
          // for this location, as stored in the shadowmap. When the closest
          // distance to the light source was saved in the shadowmap, some
          // precision was lost. Therefore we need a small tolerance factor to
          // compensate for the lost precision.
          if ( vertex_relative_to_light.z <= shadowmap_distance + u_Tolerance_constant ) {
            // This surface receives full light because it is the closest surface
            // to the light.
            return false;
          } else {
            // This surface is in a shadow because there is a closer surface to
            // the light source.
            return true;
          }
        }

        void main () {
            vec3 diffuse = v_color;
            vec3 normal = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));

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


            
//            float shadow = shadowCalculation(shadowMapTransform * vec4(v_fragPos, 1.0), shadowMap, 0.6);
//            shadow = 0.5 + (shadow * 0.5);
            
            float shadow = 1.0;
            
            if (in_shadow()) {
                shadow = 0.2;
            }
            
            lighting = 0.3 + (lighting * 0.7);
            

//            gl_FragColor =  vec4(shadow * lighting * diffuse, 1.0);
            
//            gl_FragColor =  vec4(shadow * lighting * diffuse, 1.0);



//            gl_FragColor =  vec4(lighting * diffuse, 1.0);


            
            
        
            // The vertex location rendered from the light source is almost in Normalized
            // Device Coordinates (NDC), but the perspective division has not been
            // performed yet. Perform the perspective divide. The (x,y,z) vertex location
            // components are now each in the range [-1.0,+1.0].
            vec3 vertex_relative_to_light = v_Vertex_relative_to_light.xyz / v_Vertex_relative_to_light.w;
            
            // Convert the the values from Normalized Device Coordinates (range [-1.0,+1.0])
            // to the range [0.0,1.0].
            vertex_relative_to_light = vertex_relative_to_light * 0.5 + 0.5;
            
            // Get the z value of this fragment in relationship to the light source.
            // This value was stored in the shadow map (depth buffer of the frame buffer)
            // which was passed to the shader as a texture map.
            vec4 shadowmap_color = texture2D(shadowMap, vertex_relative_to_light.xy);
            
            // The texture map contains a single depth value for each pixel. However,
            // the texture2D sampler always returns a color from a texture. For a
            // gl.DEPTH_COMPONENT texture, the color contains the depth value in
            // each of the color components. If the value was d, then the color returned
            // is (d,d,d,1). This is a "color" (depth) value between [0.0,+1.0].
            float shadowmap_distance = shadowmap_color.r;
            
            
            // Test the distance between this fragment and the light source as
            // calculated using the shadowmap transformation (vertex_relative_to_light.z) and
            // the smallest distance between the closest fragment to the light source
            // for this location, as stored in the shadowmap. When the closest
            // distance to the light source was saved in the shadowmap, some
            // precision was lost. Therefore we need a small tolerance factor to
            // compensate for the lost precision.
            //          if ( vertex_relative_to_light.z <= shadowmap_distance + tolerance ) {
            //            // This surface receives full light because it is the closest surface
            //            // to the light.
            ////            return false;
            //            shadowmap_distance = 0.0;
            //          } else {
            //            // This surface is in a shadow because there is a closer surface to
            //            // the light source.
            //            shadowmap_distance = 0.2;
            //          }
            
            //          gl_FragColor = vec4(shadowmap_distance);
            //          gl_FragColor = shadowmap_color;
            float i = 0.0;
            i = shadowmap_distance;
            
            gl_FragColor = vec4(vec3(shadowmap_distance), 1.0);
        }
    |]
