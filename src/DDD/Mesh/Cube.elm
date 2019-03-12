module DDD.Mesh.Cube exposing (mesh)

import DDD.Data.Color as Color
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives exposing (face)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)


mesh : Float -> Float -> Float -> Mesh Vertex
mesh w h l =
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
