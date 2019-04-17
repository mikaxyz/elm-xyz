module DDD.Scene.Varyings exposing (Varyings)

import Math.Vector3 exposing (Vec3)


type alias Varyings =
    { vcolor : Vec3
    , vnormal : Vec3
    , vposition : Vec3
    , vlighting : Vec3
    }