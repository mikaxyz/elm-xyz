module Scenes.Landscape exposing (init, sceneOptions)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Mesh.Primitives
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Noise
import Random
import WebGL exposing (Mesh, Shader)



--


c1 =
    { x = -3, y = 3 }


c2 =
    { x = 3, y = -3 }


segments =
    32


stepX =
    toFloat (c1.x - c2.x) / segments


stepY =
    toFloat (c1.y - c2.y) / segments


height =
    0.4


init : Scene
init =
    let
        vMap =
            List.range 0 ((segments - 1) * (segments - 1) - 1)
                |> List.map (quad (segments - 1))
                |> List.concat
    in
    { defaultScene
        | graph =
            (points segments (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                |> List.map (addElevation height)
                |> List.map (\( v, n ) -> Vertex (Color.vec3 Color.green) v n)
                |> (\vertices -> WebGL.indexedTriangles vertices vMap)
                |> Object.withMesh
                |> Object.withVertexShader vertexShader
                |> Object.withFragmentShader fragmentShader
                |> (\obj -> Graph obj [])
            )
                :: (points (segments // 4) (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                        |> List.map (addElevation height)
                        |> List.map Tuple.first
                        |> List.map bone
                   )
        , camera = Mat4.makeLookAt (vec3 0 8 5) (vec3 0 0 0) (vec3 0 1 0)
    }


quad : Int -> Int -> List ( Int, Int, Int )
quad div i =
    let
        rr =
            (i // div)
                * (div + 1)

        ii =
            modBy div i + rr
    in
    [ ( ii, ii + 1, ii + div + 1 )
    , ( ii + 1, ii + div + 1, ii + div + 2 )
    ]


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }


points : Int -> Vec2 -> Vec2 -> List Vec2
points div v1 v2 =
    List.range 0 (div * div - 1)
        |> List.map
            (\i ->
                let
                    ( ix, iy ) =
                        ( toFloat (modBy div i), toFloat (i // div) )

                    x =
                        Vec2.getX v1 + (ix / toFloat (div - 1) * (Vec2.getX v2 - Vec2.getX v1))

                    y =
                        Vec2.getY v1 + (iy / toFloat (div - 1) * (Vec2.getY v2 - Vec2.getY v1))
                in
                vec2 x y
            )


addElevation : Float -> Vec2 -> ( Vec3, Vec3 )
addElevation m v =
    let
        ( perm, newSeed_ ) =
            Noise.permutationTable (Random.initialSeed 42)

        elevation =
            Noise.noise2d perm (Vec2.getX v) (Vec2.getY v)

        elevationNorth =
            Noise.noise2d perm (Vec2.getX v) (Vec2.getY v + stepY)

        elevationSouth =
            Noise.noise2d perm (Vec2.getX v) (Vec2.getY v - stepY)

        nv1 =
            Vec3.sub
                (vec3 (Vec2.getX v) elevationNorth (Vec2.getY v + stepY))
                (vec3 (Vec2.getX v) elevationSouth (Vec2.getY v - stepY))

        elevationWest =
            Noise.noise2d perm (Vec2.getX v + stepX) (Vec2.getY v)

        elevationEast =
            Noise.noise2d perm (Vec2.getX v - stepX) (Vec2.getY v)

        nv2 =
            Vec3.sub
                (vec3 (Vec2.getX v) elevationWest (Vec2.getY v + stepX))
                (vec3 (Vec2.getX v) elevationEast (Vec2.getY v - stepX))

        normal =
            Vec3.cross nv1 nv2

        pos =
            vec3
                (Vec2.getX v)
                ((elevation * m) + m)
                (Vec2.getY v)
    in
    ( pos, normal )


bone : Vec3 -> Graph
bone v =
    DDD.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY v)
        |> WebGL.triangles
        |> Object.withMesh
        |> Object.withPosition (Vec3.setY 0 v)
        |> (\obj -> Graph obj [])


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

            highp vec3 ambientLight = vec3(0.1, 0.1, 0.1);
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
