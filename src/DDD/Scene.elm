module DDD.Scene exposing
    ( Options
    , Scene
    , cameraRotate
    , cameraRotateApply
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import DDD.Data.Vertex exposing (Vertex)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


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
    , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 1.5 0) (vec3 0 1 0)
    , cameraRotate = Mat4.identity
    }


type alias Options =
    { rotation : Float -> Mat4
    , translate : Float -> Mat4
    , perspective : Float -> Mat4
    }


defaultOptions : Options
defaultOptions =
    { rotation = \theta -> Mat4.makeRotate (6 * theta) (vec3 0 1 0)
    , translate = always Mat4.identity
    , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
    }


cameraRotate : Vec2 -> Scene -> Scene
cameraRotate rotate scene =
    { scene
        | cameraRotate =
            Mat4.makeRotate (Vec2.getX rotate) (vec3 0 1 0)
                |> Mat4.rotate (Vec2.getY rotate) (vec3 1 0 0)
    }


cameraRotateApply : Scene -> Scene
cameraRotateApply scene =
    { scene
        | camera = Mat4.mul scene.camera scene.cameraRotate
        , cameraRotate = Mat4.identity
    }


render : { width : Int, height : Int } -> Float -> Maybe Options -> Scene -> List Entity
render viewport theta options scene =
    let
        uniforms : Float -> Mat4 -> Options -> Uniforms
        uniforms aspectRatio camera options_ =
            { rotation = options_.rotation theta
            , translate = options_.translate theta
            , perspective = options_.perspective aspectRatio
            , camera = camera
            , shade = 1.0
            , light1 = lightPosition1
            , light2 = lightPosition2
            }
    in
    renderGraph
        theta
        (uniforms
            (toFloat viewport.width / toFloat viewport.height)
            (Mat4.mul scene.camera scene.cameraRotate)
            (options |> Maybe.withDefault defaultOptions)
        )
        scene.graph


renderGraph : Float -> Uniforms -> List Graph -> List Entity
renderGraph theta uniforms graph =
    graph
        |> List.map
            (\g ->
                case g of
                    Graph object children ->
                        let
                            uniforms_ =
                                { uniforms
                                    | translate =
                                        Mat4.makeTranslate (Object.position object)

                                    --                                        Mat4.mul
                                    --                                            uniforms.translate
                                    --                                            (Mat4.makeTranslate object.position)
                                    , rotation =
                                        --                                        object.rotation
                                        Mat4.mul
                                            uniforms.rotation
                                            (Object.rotationInTime theta object)
                                }
                        in
                        entity uniforms_ object
                            :: renderGraph theta uniforms_ children
            )
        |> List.concat


entity : Uniforms -> Object -> Entity
entity uniforms object =
    WebGL.entity
        (Object.vertexShader object |> Maybe.withDefault vertexShader)
        (Object.fragmentShader object |> Maybe.withDefault fragmentShader)
        (Object.mesh object)
        uniforms


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;

        void main () {
            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
            vcolor = color;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;

        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
