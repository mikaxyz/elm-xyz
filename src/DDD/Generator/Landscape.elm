module DDD.Generator.Landscape exposing (Options, addElevation, bone, mesh, points, quad)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import Math.Matrix4 as Mat4
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Noise
import Random
import WebGL exposing (Mesh, Shader)



--


type alias Options =
    { width : Float
    , height : Float
    , elevation : Float
    , segments : Int
    }


mesh : Options -> Mesh Vertex
mesh options =
    let
        c1 =
            { x = -options.width, y = options.width }

        c2 =
            { x = options.height, y = -options.height }

        vMap =
            List.range 0 ((options.segments - 1) * (options.segments - 1) - 1)
                |> List.map (quad (options.segments - 1))
                |> List.concat

        vertices =
            points options.segments (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                |> List.map (addElevation options)
                |> List.map (\( v, n ) -> Vertex (Color.vec3 Color.green) v n)

        normalBone : Vertex -> Graph
        normalBone v =
            WebGL.lines [ ( v, { v | position = Vec3.add v.position v.normal } ) ]
                |> Object.withMesh
                |> (\obj -> Graph obj [])

        normalBones =
            vertices
                |> List.map (\v -> { v | color = vec3 0.2 0.2 0.2 })
                |> List.map normalBone

        elevationBones =
            points (options.segments // 2) (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                |> List.map (addElevation options)
                |> List.map Tuple.first
                |> List.map bone

        helpers =
            normalBones

        --                ++ elevationBones
    in
    WebGL.indexedTriangles vertices vMap


bone : Vec3 -> Graph
bone v =
    DDD.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY v)
        |> WebGL.triangles
        |> Object.withMesh
        |> Object.withPosition (Vec3.setY 0 v)
        |> (\obj -> Graph obj [])


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


addElevation : Options -> Vec2 -> ( Vec3, Vec3 )
addElevation options v =
    let
        ( stepX, stepY ) =
            ( options.width / toFloat options.segments
            , options.height / toFloat options.segments
            )

        ( perm, newSeed_ ) =
            Noise.permutationTable (Random.initialSeed 42)

        f x y =
            --            m * cos x * sin y
            options.elevation * Noise.noise2d perm ((x + 100) / 6) ((y + 100) / 6)

        elevation =
            f (Vec2.getX v) (Vec2.getY v)

        elevationNorth =
            f (Vec2.getX v) (Vec2.getY v + stepY)

        elevationSouth =
            f (Vec2.getX v) (Vec2.getY v - stepY)

        elevationWest =
            f (Vec2.getX v + stepX) (Vec2.getY v)

        elevationEast =
            f (Vec2.getX v - stepX) (Vec2.getY v)

        normal =
            vec3
                ((elevationEast - elevationWest) / stepX)
                2
                ((elevationSouth - elevationNorth) / stepY)

        pos =
            vec3
                (Vec2.getX v)
                elevation
                (Vec2.getY v)
    in
    ( pos, normal |> Vec3.normalize |> Vec3.scale 0.5 )
