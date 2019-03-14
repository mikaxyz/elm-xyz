module DDD.Scene.Object exposing (Object, withMesh)

import DDD.Data.Vertex exposing (Vertex)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Mesh)


type alias Object =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    }


withMesh : Mesh Vertex -> Object
withMesh mesh =
    { position = Vec3.vec3 0 0 0
    , rotation = Mat4.identity
    , mesh = mesh
    }
