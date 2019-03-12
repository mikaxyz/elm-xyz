module DDD.Mesh.Cube exposing (mesh)

import DDD.Data.Color as Color
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives exposing (face)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)


mesh : Float -> Mesh Vertex
mesh height =
    let
        rft =
            vec3 1 height 1

        lft =
            vec3 -1 height 1

        lbt =
            vec3 -1 -height 1

        rbt =
            vec3 1 -height 1

        rbb =
            vec3 1 -height -1

        rfb =
            vec3 1 height -1

        lfb =
            vec3 -1 height -1

        lbb =
            vec3 -1 -height -1
    in
    [ face Color.green rft rfb rbb rbt
    , face Color.cyan rft rfb lfb lft
    , face Color.red rft lft lbt rbt
    , face Color.yellow rfb lfb lbb rbb
    , face Color.blue lft lfb lbb lbt
    , face Color.magenta rbt rbb lbb lbt
    ]
        |> List.concat
        |> WebGL.triangles
