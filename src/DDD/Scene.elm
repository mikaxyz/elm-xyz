module DDD.Scene exposing (Scene, render)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene.Graph exposing (Graph)
import DDD.Scene.Object exposing (Object)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


type alias Scene =
    { graph : Graph Object
    , camera : Mat4
    }


render : Entity
render =
    WebGL.entity
        vertexShader
        fragmentShader
        (DDD.Mesh.Cube.mesh 0.02)
        (sceneUniforms
            0.0
            (Mat4.makeLookAt (vec3 0 1 4) (vec3 0 1 0) (vec3 0 1 0))
        )



--


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


sceneUniforms : Float -> Mat4 -> Uniforms
sceneUniforms theta camera =
    { rotation = Mat4.makeRotate (2 * theta) (vec3 0 1 0)
    , perspective =
        Mat4.makePerspective
            45
            1
            0.01
            100
    , camera = camera
    , shade = 1.0
    }


viewport =
    { width = 800
    , height = 600
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
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
