module DDD.Scene exposing
    ( Options
    , Scene
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import DDD.Data.Vertex exposing (Vertex)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


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


render : { width : Int, height : Int } -> Vec2 -> Float -> Maybe Options -> Scene -> List Entity
render viewport drag theta options scene =
    let
        uniforms : Float -> Mat4 -> Options -> Uniforms
        uniforms aspectRatio camera options_ =
            { rotation = options_.rotation theta
            , translate = Mat4.identity
            , perspective = options_.perspective aspectRatio
            , camera = camera
            , directionalLight = directionalLight
            , worldMatrix = Mat4.identity
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
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 worldMatrix;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {
            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);
            vcolor = color;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
