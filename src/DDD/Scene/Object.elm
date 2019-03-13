module DDD.Scene.Object exposing (Object)

import DDD.Data.Vertex exposing (Vertex)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Mesh)


type alias Object =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    }
