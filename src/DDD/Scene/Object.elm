module DDD.Scene.Object exposing (Object)

import Math.Vector3 exposing (Vec3)
import WebGL exposing (Mesh)


type alias Object =
    { mesh : Mesh Int
    , position : Vec3
    , rotation : Vec3
    }
