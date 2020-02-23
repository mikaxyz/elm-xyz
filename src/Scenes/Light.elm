module Scenes.Light exposing (init, sceneOptions)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene as Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Shader)


speed =
    48


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.colorful 2 0.2 2
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -1 0)
                    |> Object.withOptionDragToRotateY
                    |> Object.withVertexShader vertexShader
                    |> Object.withFragmentShader fragmentShader
                )
                [ Graph
                    (DDD.Mesh.Cube.gray 1 1 1
                        |> Object.withMesh
                        |> Object.withPosition (vec3 0 0.6 -0.5)
                        |> Object.withOptionDragToRotateX
                        |> Object.withVertexShader vertexShader
                        |> Object.withFragmentShader fragmentShader
                    )
                    [ Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.2 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0.6 0)
                            --                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 1 0))
                            |> Object.withVertexShader vertexShader
                            |> Object.withFragmentShader fragmentShader
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0.6 0 0)
                            --                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 1 0 0))
                            |> Object.withVertexShader vertexShader
                            |> Object.withFragmentShader fragmentShader
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 -0.6 0 0)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 -1 0 0))
                            |> Object.withVertexShader vertexShader
                            |> Object.withFragmentShader fragmentShader
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.4 0.2
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0 0.6)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 1))
                            |> Object.withVertexShader vertexShader
                            |> Object.withFragmentShader fragmentShader
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.4 0.2
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0 -0.6)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 -1))
                            |> Object.withVertexShader vertexShader
                            |> Object.withFragmentShader fragmentShader
                        )
                        []
                    ]
                ]
            , light Scene.lightPosition1
            , light Scene.lightPosition2
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 4) (vec3 0 0 0) (vec3 0 1 0)
    }


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
        uniform mat4 worldMatrix;
        uniform vec3 directionalLight;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {

            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);

            highp vec3 ambientLight = vec3(0, 0, 0);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = worldMatrix * vec4(normal, 0.0);
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

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
            gl_FragColor = vec4(vcolor * vlighting, 1.0);
        }
    |]



--


lightFragmentShader : Shader {} Uniforms Varyings
lightFragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
            gl_FragColor = vec4(vcolor * vlighting, 1.0);
        }
    |]
