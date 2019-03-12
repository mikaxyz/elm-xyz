module DDD.Mesh.Tree exposing (branch, mesh)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Primitives exposing (bone, face)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)


mesh : List Int -> Mesh Vertex
mesh tree =
    tree
        |> List.map toFloat
        |> List.map (\x -> x * 0.2)
        |> List.map (branch 0.02)
        |> List.concat
        |> WebGL.triangles


branch : Float -> Float -> List ( Vertex, Vertex, Vertex )
branch thickness length =
    [ bone Color.cyan Color.magenta thickness length
    ]
        |> List.concat
