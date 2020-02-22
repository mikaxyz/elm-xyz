module DDD.Mesh.Primitives exposing (bone, bone2, face, faceWithNormal)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex as Vertex exposing (Vertex)
import Math.Vector3 exposing (Vec3, vec3)


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex.vertex position
                |> Vertex.withNormal (vec3 0 1 0)
                |> Vertex.withColor (Color.vec3 color)
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


faceWithNormal : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
faceWithNormal color a b c d normal =
    let
        vertex position =
            Vertex.vertex position
                |> Vertex.withNormal normal
                |> Vertex.withColor (Color.vec3 color)
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


bone : Color -> Color -> Float -> Float -> List ( Vertex, Vertex, Vertex )
bone color1 color2 thickness length =
    bone2 color1 color2 color2 thickness length


bone2 : Color -> Color -> Color -> Float -> Float -> List ( Vertex, Vertex, Vertex )
bone2 color1 color2 color3 thickness length =
    let
        top =
            vec3 0 length 0

        bottomL1 =
            vec3 0 0 -thickness

        bottomR1 =
            vec3 0 0 thickness

        bottomL2 =
            vec3 -thickness 0 0

        bottomR2 =
            vec3 thickness 0 0

        vertex color position =
            Vertex.vertex position
                |> Vertex.withNormal (vec3 0 1 0)
                |> Vertex.withColor (Color.vec3 color)
    in
    [ ( vertex color1 top, vertex color2 bottomL1, vertex color2 bottomR1 )
    , ( vertex color1 top, vertex color3 bottomL2, vertex color3 bottomR2 )
    ]
