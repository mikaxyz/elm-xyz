module DDD.Mesh.Cube exposing (gray, mesh)

import DDD.Data.Color as Color
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives exposing (face, faceWithNormal)
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
    , face Color.magenta rft rfb lfb lft
    , face Color.cyan rft lft lbt rbt
    , face Color.blue rfb lfb lbb rbb
    , face Color.yellow lft lfb lbb lbt
    , face Color.red rbt rbb lbb lbt
    ]
        |> List.concat
        |> WebGL.triangles


gray : Float -> Float -> Float -> Mesh Vertex
gray w h l =
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
            faceWithNormal Color.grey50 rft lft lbt rbt (vec3 0 0 1)

        back =
            faceWithNormal Color.grey50 rfb lfb lbb rbb (vec3 0 0 -1)

        left =
            faceWithNormal Color.grey50 lft lfb lbb lbt (vec3 -1 0 0)

        right =
            faceWithNormal Color.grey50 rft rfb rbb rbt (vec3 1 0 0)

        top =
            faceWithNormal Color.grey50 rft rfb lfb lft (vec3 0 1 0)

        bottom =
            faceWithNormal Color.grey50 rbt rbb lbb lbt (vec3 0 -1 0)
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
