module XYZMika.XYZ.Mesh.Cube exposing (colorful, gray, withColor)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (..)
import XYZMika.Color as Color exposing (Color)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Primitives exposing (faceWithNormal)


type alias Colors =
    { top : Color
    , bottom : Color
    , left : Color
    , right : Color
    , front : Color
    , back : Color
    }


colorful : Float -> Float -> Float -> Mesh Vertex
colorful =
    cube
        (Colors
            Color.green
            Color.magenta
            Color.cyan
            Color.blue
            Color.yellow
            Color.red
        )


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
    cube
        (Colors
            color
            color
            color
            color
            color
            color
        )


cube : Colors -> Float -> Float -> Float -> Mesh Vertex
cube colors w h l =
    let
        rft =
            vec3 w h l |> Vec3.scale 0.5

        lft =
            vec3 -w h l |> Vec3.scale 0.5

        lbt =
            vec3 -w -h l |> Vec3.scale 0.5

        rbt =
            vec3 w -h l |> Vec3.scale 0.5

        rbb =
            vec3 w -h -l |> Vec3.scale 0.5

        rfb =
            vec3 w h -l |> Vec3.scale 0.5

        lfb =
            vec3 -w h -l |> Vec3.scale 0.5

        lbb =
            vec3 -w -h -l |> Vec3.scale 0.5

        front =
            faceWithNormal colors.front rft lft lbt rbt (vec3 0 0 1)

        back =
            faceWithNormal colors.back rfb lfb lbb rbb (vec3 0 0 -1)

        left =
            faceWithNormal colors.left lft lfb lbb lbt (vec3 -1 0 0)

        right =
            faceWithNormal colors.right rft rfb rbb rbt (vec3 1 0 0)

        top =
            faceWithNormal colors.top rft rfb lfb lft (vec3 0 1 0)

        bottom =
            faceWithNormal colors.bottom rbt rbb lbb lbt (vec3 0 -1 0)
    in
    [ front
    , back
    , left
    , right
    , top
    , bottom
    ]
        |> List.concat
        |> WebGL.triangles
