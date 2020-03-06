module XYZMika.XYZ.Mesh.Cube exposing
    ( colorful
    , gray
    , pairsColorfulWithBounds
    , pairsWithBounds
    , pairsWithBoundsAndColor
    , pairsWithBoundsAndColors
    , withBounds
    , withBoundsAndColor
    , withBoundsAndColors
    , withBoundsColorful
    , withColor
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (..)
import XYZMika.Color as Color exposing (Color)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Primitives exposing (quadWithNormal)


type alias Colors =
    { top : Color
    , bottom : Color
    , left : Color
    , right : Color
    , front : Color
    , back : Color
    }


colorsWhite : Colors
colorsWhite =
    Colors
        Color.white
        Color.white
        Color.white
        Color.white
        Color.white
        Color.white


colorsColorful : Colors
colorsColorful =
    Colors
        Color.green
        Color.magenta
        Color.cyan
        Color.blue
        Color.yellow
        Color.red


type alias Corners =
    { frontTopLeft : Vec3
    , frontTopRight : Vec3
    , frontBottomLeft : Vec3
    , frontBottomRight : Vec3
    , backTopLeft : Vec3
    , backTopRight : Vec3
    , backBottomLeft : Vec3
    , backBottomRight : Vec3
    }


colorful : Float -> Float -> Float -> Mesh Vertex
colorful =
    cube colorsColorful


gray : Float -> Float -> Float -> Mesh Vertex
gray =
    cube
        (Colors
            Color.grey50
            Color.grey50
            Color.grey50
            Color.grey50
            Color.grey50
            Color.grey50
        )


withColor : Color -> Float -> Float -> Float -> Mesh Vertex
withColor color =
    cube (Colors color color color color color color)


cube : Colors -> Float -> Float -> Float -> Mesh Vertex
cube colors w h l =
    withBoundsAndColors colors
        ( vec3 -w -h -l |> Vec3.scale 0.5
        , vec3 w h l |> Vec3.scale 0.5
        )
        |> WebGL.triangles


cornersWithBounds : ( Vec3, Vec3 ) -> Corners
cornersWithBounds ( v1, v2 ) =
    let
        vr1 =
            v1 |> Vec3.toRecord

        vr2 =
            v2 |> Vec3.toRecord

        vMin =
            vec3 (min vr1.x vr2.x) (min vr1.y vr2.y) (min vr1.z vr2.z)
                |> Vec3.toRecord

        vMax =
            vec3 (max vr1.x vr2.x) (max vr1.y vr2.y) (max vr1.z vr2.z)
                |> Vec3.toRecord
    in
    { frontTopLeft = vec3 vMin.x vMax.y vMax.z
    , frontTopRight = vec3 vMax.x vMax.y vMax.z
    , frontBottomLeft = vec3 vMin.x vr1.y vMax.z
    , frontBottomRight = vec3 vMax.x vr1.y vMax.z
    , backTopLeft = vec3 vMin.x vMax.y vMin.z
    , backTopRight = vec3 vMax.x vMax.y vMin.z
    , backBottomLeft = vec3 vMin.x vr1.y vMin.z
    , backBottomRight = vec3 vMax.x vr1.y vMin.z
    }


pairsWithBounds : ( Vec3, Vec3 ) -> List ( Vertex, Vertex )
pairsWithBounds =
    pairsWithBoundsAndColors colorsWhite


pairsColorfulWithBounds : ( Vec3, Vec3 ) -> List ( Vertex, Vertex )
pairsColorfulWithBounds =
    pairsWithBoundsAndColors colorsColorful


pairsWithBoundsAndColor : Color -> ( Vec3, Vec3 ) -> List ( Vertex, Vertex )
pairsWithBoundsAndColor color =
    pairsWithBoundsAndColors
        (Colors color color color color color color)


pairsWithBoundsAndColors : Colors -> ( Vec3, Vec3 ) -> List ( Vertex, Vertex )
pairsWithBoundsAndColors colors bounds =
    let
        pairWithColor : Color -> ( Vertex, Vertex ) -> ( Vertex, Vertex )
        pairWithColor color pair =
            pair
                |> Tuple.mapBoth
                    (Vertex.withColor (Color.toVec3 color))
                    (Vertex.withColor (Color.toVec3 color))

        c =
            cornersWithBounds bounds
    in
    [ -- FRONT horisontal
      pairWithColor colors.front ( Vertex.vertex c.frontTopRight, Vertex.vertex c.frontTopLeft )
    , pairWithColor colors.front ( Vertex.vertex c.frontBottomRight, Vertex.vertex c.frontBottomLeft )

    -- RIGHT horisontal
    , pairWithColor colors.right ( Vertex.vertex c.frontTopRight, Vertex.vertex c.backTopRight )
    , pairWithColor colors.right ( Vertex.vertex c.frontBottomRight, Vertex.vertex c.backBottomRight )

    -- LEFT horisontal
    , pairWithColor colors.left ( Vertex.vertex c.frontTopLeft, Vertex.vertex c.backTopLeft )
    , pairWithColor colors.left ( Vertex.vertex c.frontBottomLeft, Vertex.vertex c.backBottomLeft )

    -- BACK horisontal
    , pairWithColor colors.back ( Vertex.vertex c.backTopRight, Vertex.vertex c.backTopLeft )
    , pairWithColor colors.back ( Vertex.vertex c.backBottomRight, Vertex.vertex c.backBottomLeft )

    -- FRONT vertical
    , pairWithColor colors.front ( Vertex.vertex c.frontTopRight, Vertex.vertex c.frontBottomRight )
    , pairWithColor colors.front ( Vertex.vertex c.frontTopLeft, Vertex.vertex c.frontBottomLeft )

    -- BACK vertical
    , pairWithColor colors.back ( Vertex.vertex c.backTopRight, Vertex.vertex c.backBottomRight )
    , pairWithColor colors.back ( Vertex.vertex c.backTopLeft, Vertex.vertex c.backBottomLeft )
    ]


withBounds : ( Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
withBounds =
    withBoundsAndColors colorsWhite


withBoundsColorful : ( Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
withBoundsColorful =
    withBoundsAndColors colorsColorful


withBoundsAndColor : Color -> ( Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
withBoundsAndColor color =
    withBoundsAndColors (Colors color color color color color color)


withBoundsAndColors : Colors -> ( Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
withBoundsAndColors colors ( v1, v2 ) =
    let
        c =
            cornersWithBounds ( v1, v2 )

        front =
            quadWithNormal colors.front c.frontTopRight c.frontTopLeft c.frontBottomLeft c.frontBottomRight (vec3 0 0 1)

        back =
            quadWithNormal colors.back c.backTopLeft c.backTopRight c.backBottomRight c.backBottomLeft (vec3 0 0 -1)

        left =
            quadWithNormal colors.left c.frontTopLeft c.frontBottomLeft c.backBottomLeft c.backTopLeft (vec3 -1 0 0)

        right =
            quadWithNormal colors.right c.frontBottomRight c.frontTopRight c.backTopRight c.backBottomRight (vec3 1 0 0)

        top =
            quadWithNormal colors.top c.frontTopRight c.frontTopLeft c.backTopLeft c.backTopRight (vec3 0 1 0)

        bottom =
            quadWithNormal colors.bottom c.frontBottomRight c.frontBottomLeft c.backBottomLeft c.backBottomRight (vec3 0 -1 0)
    in
    [ front
    , back
    , left
    , right
    , top
    , bottom
    ]
        |> List.concat
