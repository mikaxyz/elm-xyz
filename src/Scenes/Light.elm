module Scenes.Light exposing (init, sceneOptions)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene as Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Shader)


opt =
    { rotation = \theta -> Mat4.makeRotate (8 * theta) (vec3 0 1 0)
    , translate = always Mat4.identity
    }


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.gray 1 1 1
                    |> Object.withMesh
                    --                    |> Object.withOptions opt
                    |> Object.withVertexShader vertexShader
                    |> Object.withFragmentShader fragmentShader
                )
                []
            , floor
            , light Scene.lightPosition1
            , light Scene.lightPosition2
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 4) (vec3 0 0 0) (vec3 0 1 0)
    }


floor =
    Graph
        (DDD.Mesh.Cube.colorful 2 0.2 2
            |> Object.withMesh
            |> Object.withPosition (vec3 0 -0.6 0)
            |> Object.withVertexShader vertexShader
            |> Object.withFragmentShader fragmentShader
        )
        []


light p =
    Graph
        (DDD.Mesh.Cube.colorful 0.02 0.02 0.02
            |> Object.withMesh
            |> Object.withPosition p
            |> Object.withFragmentShader lightFragmentShader
        )
        []


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 normal;
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;
        uniform vec3 directionalLight;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {

            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);

            highp vec3 ambientLight = vec3(0, 0, 0);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = rotation * vec4(normal, 1.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            vlighting = ambientLight + (directionalLightColor * directional);
            vcolor = color;
            vnormal = normal;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float shade;
        uniform vec3 light1;
        uniform vec3 light2;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {
            gl_FragColor = vec4(vcolor * vlighting, 1.0);
        }
    |]



--


lightFragmentShader : Shader {} Uniforms Varyings
lightFragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
