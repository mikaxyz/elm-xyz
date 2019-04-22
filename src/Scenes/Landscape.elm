module Scenes.Landscape exposing (init, sceneOptions)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Generator.Perlin as Perlin
import DDD.Mesh.Landscape
import DDD.Mesh.Primitives
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


elevation x y =
    let
        seed =
            42

        freq =
            0.5

        e1 =
            Perlin.value2d { seed = seed, freq = freq } x y
    in
    e1
        + (0.5 * e1 * max e1 0 * Perlin.value2d { seed = seed, freq = 3 * freq } x y)
        + (0.5 * e1 * max e1 0 * Perlin.value2d { seed = seed, freq = 10 * freq } x y)


init : Scene
init =
    let
        divisions =
            31

        color height =
            let
                normalized =
                    height + 1 / 2
            in
            vec3
                normalized
                1
                normalized

        landscape =
            DDD.Mesh.Landscape.simple
                { divisions = divisions
                , width = 3
                , length = 3
                , height = 1
                , color = color
                , elevation = elevation
                }

        normalBone : Vertex -> Graph
        normalBone v =
            WebGL.lines [ ( v, { v | position = Vec3.add v.position v.normal } ) ]
                |> Object.withMesh
                |> (\obj -> Graph obj [])

        normalGuides =
            landscape
                |> Tuple.first
                |> List.map
                    (\v ->
                        { v
                            | color = vec3 0.2 0.2 0.2
                            , normal = Vec3.scale 0.2 v.normal
                        }
                    )
                |> List.map normalBone

        bone : Vec3 -> Graph
        bone v =
            DDD.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY (Vec3.add (vec3 0 1 0) v))
                |> WebGL.triangles
                |> Object.withMesh
                |> Object.withPosition (Vec3.setY -1 v)
                |> (\obj -> Graph obj [])

        elevationBones density =
            landscape
                |> Tuple.first
                |> List.indexedMap
                    (\i v ->
                        if modBy density i == 0 && modBy density (i // divisions) == 0 then
                            Just v.position

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.map bone

        helpers =
            normalGuides ++ elevationBones 4
    in
    { defaultScene
        | graph =
            [ landscape
                |> (\( v, vmap ) -> WebGL.indexedTriangles v vmap)
                |> Object.withMesh
                --                |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (4 * theta) (vec3 0 1 0))
                |> Object.withOptionDragToRotateXY
                |> Object.withVertexShader vertexShader
                |> Object.withFragmentShader fragmentShader
                |> (\obj -> Graph obj helpers)
            ]
        , camera = Mat4.makeLookAt (vec3 0 4 7) (vec3 0 0 0) (vec3 0 1 0)
    }


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

        void main () {

            gl_Position = perspective * camera * worldMatrix * vec4(position, 1.0);

            highp vec3 ambientLight = vec3(0.1, 0.1, 0.1);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = worldMatrix * vec4(normalize(normal), 0.0);
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

        void main () {
            gl_FragColor = vec4(vcolor * vlighting, 1.0);
        }
    |]
