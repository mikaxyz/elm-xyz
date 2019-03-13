module DDD.Mesh.Primitives exposing (bone, bone2, face)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import Math.Vector3 exposing (Vec3, vec3)


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Color.vec3 color) position
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
            Vertex (Color.vec3 color) position
    in
    [ ( vertex color1 top, vertex color2 bottomL1, vertex color2 bottomR1 )
    , ( vertex color1 top, vertex color3 bottomL2, vertex color3 bottomR2 )
    ]
