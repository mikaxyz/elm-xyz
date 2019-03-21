module DDD.Data.Vertex exposing (Vertex, scale)

import Math.Vector3 as Vec3 exposing (Vec3)


type alias Vertex =
    { color : Vec3
    , position : Vec3
    , normal : Vec3
    }


scale x v =
    { v | position = v.position |> Vec3.scale x }
