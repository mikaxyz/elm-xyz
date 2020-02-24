module DDD.Scene exposing
    ( Options
    , Scene
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import Asset
import Asset.Store
import DDD.Data.Vertex exposing (Vertex)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


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


render : Texture -> { width : Int, height : Int } -> Vec2 -> Float -> Maybe Options -> Asset.Store.Store Asset.Obj Asset.Texture -> Scene -> List Entity
render defaultTexture viewport drag theta options assets scene =
    let
        uniforms : Float -> Mat4 -> Options -> Uniforms
        uniforms aspectRatio camera options_ =
            { rotation = options_.rotation theta
            , translate = Mat4.identity
            , perspective = options_.perspective aspectRatio
            , camera = camera
            , directionalLight = directionalLight
            , worldMatrix = Mat4.identity
            , texture = defaultTexture
            , hasTextureMap = False
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

                            translate =
                                uniforms.translate
                                    |> Mat4.translate (Object.position object_)

                            rotation =
                                Object.rotation object_
                                    |> Mat4.mul uniforms.rotation

                            worldMatrix =
                                Object.rotation object_
                                    |> Mat4.mul (Mat4.makeTranslate (Object.position object_))
                                    |> Mat4.mul uniforms.worldMatrix

                            uniforms_ =
                                { uniforms
                                    | translate = translate
                                    , rotation = rotation
                                    , worldMatrix = worldMatrix
                                    , texture = object_ |> Object.textureWithDefault uniforms.texture
                                    , hasTextureMap = Object.textureMap object_ /= Nothing
                                    , normalMap = object_ |> Object.normalMapWithDefault uniforms.normalMap
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


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        attribute vec2 uv;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 translate;
        uniform mat4 rotation;
        uniform mat4 worldMatrix;
        uniform vec3 directionalLight;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            vcolor = color;
            vcoord = uv;
            vnormal = normal;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        uniform sampler2D normalMap;
        uniform bool hasNormalMap;
        uniform bool hasTextureMap;
        uniform vec3 directionalLight;
        uniform mat4 worldMatrix;
        uniform float normalMapIntensity;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
            vec3 tex;
            if(hasTextureMap) {
                tex = texture2D(texture, vcoord).rgb;
            } else {
                tex = vec3(1, 1, 1);
            }
            
            vec3 normal;
            if(hasNormalMap) {
                normal = texture2D(normalMap, vcoord).rgb;
                normal = vnormal * normalize(normal) * normalMapIntensity;
            } else {
                normal = vnormal;
            }
            
            // Lighting
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = worldMatrix * vec4(normal, 0.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
    
            vec3 f_lighting = (directionalLightColor * directional);

            gl_FragColor =  vec4(vcolor * tex * f_lighting, 1.0);
        }
    |]
