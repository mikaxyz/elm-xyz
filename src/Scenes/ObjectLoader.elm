module Scenes.ObjectLoader exposing (addMesh, getObj, init, sceneOptions)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Http
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Shader)


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.gray 2 0.1 2
                    |> Object.withMesh
                    |> Object.withVertexShader vertexShader
                    |> Object.withFragmentShader fragmentShader
                    |> Object.withPosition (vec3 0 -0.75 0)
                    |> Object.withOptionDragToRotateXY
                )
                []
            ]
        , camera = Mat4.makeLookAt (vec3 0 0 2.5) (vec3 0 0 0) (vec3 0 1 0)
    }


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }


getObj : { scale : Float, color : Vec3 } -> Vec3 -> String -> (( { scale : Float, color : Vec3 }, Vec3, String ) -> msg) -> Cmd msg
getObj options pos url tagger =
    Http.get
        { url = url
        , expect = Http.expectString (\x -> tagger ( options, pos, x |> Result.withDefault "" ))
        }


addMesh : List ( Vertex, Vertex, Vertex ) -> Vec3 -> Scene -> Scene
addMesh tris pos scene =
    let
        graphObject : Object
        graphObject =
            tris
                |> WebGL.triangles
                |> Object.withMesh
                |> Object.withVertexShader vertexShader
                |> Object.withFragmentShader fragmentShader
                |> Object.withPosition pos

        updated =
            case scene.graph of
                graph :: _ ->
                    case graph of
                        Graph root children ->
                            [ Graph root (Graph graphObject [] :: children) ]

                _ ->
                    scene.graph
    in
    { scene | graph = updated }


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
