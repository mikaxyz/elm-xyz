module Scenes.Landscape exposing (init, sceneOptions)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import Math.Matrix4 as Mat4
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Noise
import Random
import WebGL exposing (Mesh)



--


c1 =
    { x = -3, y = 3 }


c2 =
    { x = 3, y = -3 }


init : Scene
init =
    { defaultScene
        | graph =
            plane (vec3 c1.x 0 c1.y) (vec3 c2.x 0 c2.y)
                :: (points 20 (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                        |> List.map pointData
                        |> List.map (\p -> vec3 p.x p.y p.z)
                        |> List.map handle
                   )
        , camera = Mat4.makeLookAt (vec3 0 2 5) (vec3 0 0 0) (vec3 0 1 0)
    }


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


type alias Data =
    { x : Float
    , y : Float
    , z : Float
    }


pointData : Vec2 -> Data
pointData v =
    let
        ( perm, newSeed_ ) =
            Noise.permutationTable (Random.initialSeed 42)

        elevation =
            Noise.noise3d perm (Vec2.getX v) (Vec2.getY v) 1
    in
    { x = Vec2.getX v
    , y = (elevation * 0.3) + 0.3
    , z = Vec2.getY v
    }


handle : Vec3 -> Graph
handle v =
    DDD.Mesh.Cube.mesh 0.1 0.1 0.1
        |> Object.withMesh
        |> Object.withPosition v
        |> (\obj -> Graph obj [])


plane : Vec3 -> Vec3 -> Graph
plane v1 v2 =
    corners v1 v2
        |> face (Color.vec3 Color.grey50)
        |> WebGL.triangles
        |> Object.withMesh
        |> (\obj -> Graph obj [])


handles : Vec3 -> Vec3 -> List Graph
handles v1 v2 =
    corners v1 v2
        |> (\x ->
                [ x.tl, x.tr, x.br, x.bl ]
                    |> List.map
                        (\pos ->
                            DDD.Mesh.Cube.mesh 0.1 0.1 0.1
                                |> Object.withMesh
                                |> Object.withPosition pos
                        )
                    |> List.map (\obj -> Graph obj [])
           )


corners : Vec3 -> Vec3 -> { tl : Vec3, br : Vec3, tr : Vec3, bl : Vec3 }
corners tl br =
    { tl = tl
    , br = br
    , tr = Vec3.cross br Vec3.j
    , bl = Vec3.cross tl Vec3.j
    }


face : Vec3 -> { tl : Vec3, br : Vec3, tr : Vec3, bl : Vec3 } -> List ( Vertex, Vertex, Vertex )
face color v =
    let
        vertex position =
            Vertex color position
    in
    [ ( vertex v.tl, vertex v.tr, vertex v.br )
    , ( vertex v.tl, vertex v.br, vertex v.bl )
    ]
