module Scenes.Landscape exposing (init, sceneOptions)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Mesh.Primitives
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
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
    40


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
                |> List.map (addElevation 0.5)
                |> List.map (\v -> Vertex (Color.vec3 Color.green) v (vec3 0 1 0))
                |> (\vertices -> WebGL.indexedTriangles vertices vMap)
                |> Object.withMesh
                |> Object.withVertexShader vertexShader
                |> Object.withFragmentShader fragmentShader
                |> (\obj -> Graph obj [])
            )
                :: (points (segments // 4) (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                        |> List.map (addElevation 0.5)
                        |> List.map bone
                   )
        , camera = Mat4.makeLookAt (vec3 0 8 -5) (vec3 0 0 0) (vec3 0 1 0)
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


addElevation : Float -> Vec2 -> Vec3
addElevation m v =
    let
        ( perm, newSeed_ ) =
            Noise.permutationTable (Random.initialSeed 42)

        elevation =
            Noise.noise3d perm (Vec2.getX v + 100) (Vec2.getY v + 100) 1
    in
    vec3
        (Vec2.getX v)
        ((elevation * m) + m)
        (Vec2.getY v)


bone : Vec3 -> Graph
bone v =
    DDD.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY v)
        |> WebGL.triangles
        |> Object.withMesh
        |> Object.withPosition (Vec3.setY 0 v)
        |> (\obj -> Graph obj [])


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 normal;
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        //varying vec3 vnormal;
        //f_position = vec3(mvp * vec4(position, 1.0));

        void main () {
            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
            vcolor = color;
            vnormal = normal;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float shade;
        uniform vec3 light1;
        uniform vec3 light2;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;

        // vec3 sun = light1;
        float lightintensity1 = 0.0;
        float lightintensity2 = 0.0;

        void main () {
            vec3 l1 = normalize(light1 - vposition);
            vec3 l2 = normalize(light2 - vposition);
            //light = max(dot(vnormal, light), 0.0) * vec3(1.0, 1.0, 1.0);

            //lightintensity1 = dot(vnormal, l1);
            //lightintensity2 = dot(vnormal, l2);
            lightintensity1 = max(dot(vnormal, l1), 0.0);
            lightintensity2 = max(dot(vnormal, l2), 0.0);

            //gl_FragColor = shade * vec4(vcolor, light);
            gl_FragColor = (lightintensity1 + lightintensity2) * vec4(vcolor, 1.0);

            //vec3 light = normalize(sun);
            //gl_FragColor = shade * vec4(vcolor, light);
        }
    |]
