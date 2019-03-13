module DDD.Scene exposing
    ( Scene
    , cameraRotate
    , cameraRotateApply
    , init
    , render
    )

--import DDD.Mesh.Tree exposing (tree)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


type alias Scene =
    { graph : List Graph
    , camera : Mat4
    , cameraRotate : Mat4
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


floor : Object
floor =
    { position = vec3 0 0 0
    , rotation = Mat4.identity
    , mesh = DDD.Mesh.Cube.mesh 1 0.1 1
    }



--


cubes : Float -> Float -> List Graph
cubes r w =
    List.range 0 (round (3 / w))
        |> List.map toFloat
        |> List.map
            (\i ->
                Graph
                    { position = vec3 r (i * w + 0.1) 0
                    , rotation = Mat4.makeRotate (Basics.pi / 6 * i) (vec3 0 1 0)

                    --                        Mat4.mul
                    --                        Mat4.makeRotate (Basics.pi / 6 * i) (vec3 0 1 0)
                    --                            (Mat4.makeTranslate (vec3 r (i * w + 0.1) 0))
                    , mesh = DDD.Mesh.Cube.mesh w 0.01 w
                    }
                    []
            )


init : Scene
init =
    { graph =
        Graph floor []
            :: cubes 0.8 0.1

    --            ++ cubes 0.6 0.075
    --            ++ cubes 0.4 0.05
    --            ++ cubes 0.2 0.025
    --            ++ cubes 0.1 0.005
    --            ++ tree
    , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 1.5 0) (vec3 0 1 0)
    , cameraRotate = Mat4.identity
    }


render : { width : Int, height : Int } -> Float -> Scene -> List Entity
render viewport theta scene =
    let
        uniforms : Float -> Mat4 -> Uniforms
        uniforms aspectRatio camera =
            { rotation = Mat4.makeRotate (6 * theta) (vec3 0 1 0)
            , translate = Mat4.identity
            , perspective = Mat4.makePerspective 45 aspectRatio 0.01 100
            , camera = camera
            , shade = 1.0
            }
    in
    renderGraph
        (uniforms
            (toFloat <| viewport.width // viewport.height)
            (Mat4.mul scene.camera scene.cameraRotate)
        )
        scene.graph


renderGraph : Uniforms -> List Graph -> List Entity
renderGraph uniforms graph =
    graph
        |> List.map
            (\g ->
                case g of
                    Graph object children ->
                        let
                            uniforms_ =
                                { uniforms
                                    | translate =
                                        Mat4.mul
                                            uniforms.translate
                                            (Mat4.makeTranslate object.position)
                                    , rotation =
                                        Mat4.mul
                                            object.rotation
                                            uniforms.rotation
                                }
                        in
                        entity uniforms_ object
                            :: renderGraph uniforms_ children
            )
        |> List.concat


entity : Uniforms -> Object -> Entity
entity uniforms object =
    WebGL.entity
        vertexShader
        fragmentShader
        object.mesh
        uniforms


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
