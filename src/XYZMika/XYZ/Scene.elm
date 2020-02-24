module XYZMika.XYZ.Scene exposing
    ( Options
    , Scene
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


directionalLight =
    Vec3.fromRecord { x = 1, y = 0.7, z = 0.5 }


lightPosition1 =
    vec3 -4 3 3


lightPosition2 =
    vec3 -6 2 3.5


type alias Scene =
    { graph : List Graph
    , camera : Mat4
    , cameraRotate : Mat4
    }


defaultScene : Scene
defaultScene =
    { graph = []
    , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 0 0) (vec3 0 1 0)
    , cameraRotate = Mat4.identity
    }


type alias Options =
    { rotation : Float -> Mat4
    , translate : Float -> Mat4
    , perspective : Float -> Mat4
    }


defaultOptions : Options
defaultOptions =
    { rotation = always Mat4.identity
    , translate = always Mat4.identity
    , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
    }


render : Texture -> { width : Int, height : Int } -> Vec2 -> Float -> Maybe Options -> Scene -> List Entity
render defaultTexture viewport drag theta options scene =
    let
        uniforms : Float -> Mat4 -> Options -> Uniforms
        uniforms aspectRatio camera options_ =
            { perspective = options_.perspective aspectRatio
            , camera = camera
            , directionalLight = directionalLight
            , worldMatrix = Mat4.identity
            , diffuseMap = defaultTexture
            , hasDiffuseMap = False
            , normalMap = defaultTexture
            , hasNormalMap = False
            , normalMapIntensity = 2.0
            }
    in
    renderGraph
        drag
        theta
        (uniforms
            (toFloat viewport.width / toFloat viewport.height)
            (scene.camera
             --                |> Mat4.rotate (Vec2.getY drag * 0.01) (vec3 1 0 0)
             --                |> Mat4.rotate (Vec2.getX drag * 0.01) (vec3 0 1 0)
            )
            (options |> Maybe.withDefault defaultOptions)
        )
        scene.graph


renderGraph : Vec2 -> Float -> Uniforms -> List Graph -> List Entity
renderGraph drag theta uniforms graph =
    graph
        |> List.map
            (\g ->
                case g of
                    Graph object children ->
                        let
                            object_ =
                                object
                                    |> Object.rotationWithDrag drag
                                    |> Object.rotationInTime theta

                            worldMatrix =
                                Object.rotation object_
                                    |> Mat4.mul (Mat4.makeTranslate (Object.position object_))
                                    |> Mat4.mul uniforms.worldMatrix

                            uniforms_ =
                                { uniforms
                                    | worldMatrix = worldMatrix
                                    , diffuseMap = object_ |> Object.diffuseMapWithDefault uniforms.diffuseMap
                                    , hasDiffuseMap = Object.diffuseMap object_ /= Nothing
                                    , normalMap = object_ |> Object.normalMapWithDefault uniforms.normalMap
                                    , normalMapIntensity = object_ |> Object.normalMapIntensityWithDefault uniforms.normalMapIntensity
                                    , hasNormalMap = Object.normalMap object_ /= Nothing
                                }
                        in
                        entity uniforms_ object_
                            :: renderGraph drag theta uniforms_ children
            )
        |> List.concat


entity : Uniforms -> Object -> Entity
entity uniforms object =
    WebGL.entity
        (Object.vertexShader object |> Maybe.withDefault vertexShader)
        (Object.fragmentShader object |> Maybe.withDefault fragmentShader)
        (Object.mesh object)
        uniforms


vertexShader :
    Shader Vertex
        { u
            | perspective : Mat4
            , camera : Mat4
            , worldMatrix : Mat4
        }
        { v_color : Vec3
        , v_normal : Vec3
        , v_uv : Vec2
        }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        attribute vec2 uv;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;

        varying vec3 v_color;
        varying vec2 v_uv;
        varying vec3 v_normal;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            v_color = color;
            v_uv = uv;
            v_normal = normal;
        }
    |]


fragmentShader :
    Shader {}
        { u
            | worldMatrix : Mat4

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
