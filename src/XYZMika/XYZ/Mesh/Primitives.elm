module XYZMika.XYZ.Mesh.Primitives exposing
    ( bone
    , bone2
    , bone3
    , quad
    , quadWithNormal
    )

import Math.Vector2 exposing (vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.Color as Color exposing (Color)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)


quad : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
quad color a b c d =
    let
        vertex position =
            Vertex.vertex position
                |> Vertex.withNormal (vec3 0 1 0)
                |> Vertex.withColor (Color.toVec3 color)
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


quadWithNormal : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
quadWithNormal color a b c d normal =
    let
        tangent v =
            v
                |> Vec3.toRecord
                |> (\{ x, y, z } -> Vec3.vec3 z x y)

        vertex position uv =
            Vertex.vertex position
                |> Vertex.withNormal normal
                |> Vertex.withTangent (tangent normal)
                |> Vertex.withColor (Color.toVec3 color)
                |> Vertex.withUV uv
    in
    [ ( vertex a (vec2 0 0), vertex b (vec2 0 1), vertex c (vec2 1 1) )
    , ( vertex c (vec2 1 1), vertex d (vec2 1 0), vertex a (vec2 0 0) )
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
                |> Vertex.withColor (Color.toVec3 color)
    in
    [ ( vertex color1 top, vertex color2 bottomL1, vertex color2 bottomR1 )
    , ( vertex color1 top, vertex color3 bottomL2, vertex color3 bottomR2 )
    ]


bone3 : Float -> List ( Vertex, Vertex, Vertex )
bone3 length =
    let
        ( color1, color2 ) =
            ( Color.cyan, Color.magenta )

        ( top, bottom ) =
            ( vec3 0 length 0
            , vec3 0 0 0
            )

        ( ly, thickness ) =
            ( length * 0.1, length * 0.1 )

        { l1, l2, r1, r2 } =
            { l1 = vec3 0 ly -thickness
            , l2 = vec3 -thickness ly 0
            , r1 = vec3 0 ly thickness
            , r2 = vec3 thickness ly 0
            }

        vertex color position =
            Vertex.vertex position
                |> Vertex.withNormal (vec3 0 1 0)
                |> Vertex.withColor (Color.toVec3 color)
    in
    [ ( vertex color1 top, vertex color1 l1, vertex color1 l2 )
    , ( vertex color1 top, vertex color1 l2, vertex color1 r1 )
    , ( vertex color1 top, vertex color1 r1, vertex color1 r2 )
    , ( vertex color1 top, vertex color1 r2, vertex color1 l1 )

    --
    , ( vertex color2 bottom, vertex color2 l1, vertex color2 l2 )
    , ( vertex color2 bottom, vertex color2 l2, vertex color2 r1 )
    , ( vertex color2 bottom, vertex color2 r1, vertex color2 r2 )
    , ( vertex color2 bottom, vertex color2 r2, vertex color2 l1 )
    ]
