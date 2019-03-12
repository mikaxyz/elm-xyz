module DDD.Scene exposing (Scene, init, render)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


type alias Scene =
    { graph : List Graph
    , camera : Mat4
    }


type Graph
    = Graph Object (List Graph)


type alias Object =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    }


floor =
    { position = vec3 0 0 0
    , rotation = Mat4.identity
    , mesh = DDD.Mesh.Cube.mesh 1 0.1 1
    }


cube =
    { position = vec3 0.4 0.2 0
    , rotation = Mat4.makeRotate (Basics.pi / 12) (vec3 0 1 0)
    , mesh = DDD.Mesh.Cube.mesh 0.2 0.2 0.2
    }



--


init : Scene
init =
    { graph = [ Graph floor [ Graph cube [ Graph cube [ Graph cube [] ] ] ] ]
    , camera = Mat4.makeLookAt (vec3 0 1 4) (vec3 0 1 0) (vec3 0 1 0)
    }


render : { width : Int, height : Int } -> Float -> Scene -> List Entity
render viewport theta scene =
    renderGraph
        (sceneUniforms
            (toFloat <| viewport.width // viewport.height)
            theta
            (Mat4.makeLookAt (vec3 0 1 4) (vec3 0 1 0) (vec3 0 1 0))
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


sceneUniforms : Float -> Float -> Mat4 -> Uniforms
sceneUniforms aspectRatio theta camera =
    { rotation = Mat4.makeRotate (8 * theta) (vec3 0 1 0)
    , translate = Mat4.identity
    , perspective = Mat4.makePerspective 45 aspectRatio 0.01 100
    , camera = camera
    , shade = 1.0
    }


type alias Uniforms =
    { rotation : Mat4
    , translate : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


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
