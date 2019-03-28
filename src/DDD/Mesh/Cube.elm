module DDD.Mesh.Cube exposing (colorful, gray)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives exposing (face, faceWithNormal)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)


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


type alias Colors =
    { top : Color
    , bottom : Color
    , left : Color
    , right : Color
    , front : Color
    , back : Color
    }


cube : Colors -> Float -> Float -> Float -> Mesh Vertex
cube colors w h l =
    let
        rft =
            vec3 w h l

        lft =
            vec3 -w h l

        lbt =
            vec3 -w -h l

        rbt =
            vec3 w -h l

        rbb =
            vec3 w -h -l

        rfb =
            vec3 w h -l

        lfb =
            vec3 -w h -l

        lbb =
            vec3 -w -h -l

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
