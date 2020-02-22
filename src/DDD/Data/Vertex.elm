module DDD.Data.Vertex exposing (..)

import Math.Vector3 exposing (Vec3, vec3)


type alias Vertex =
    { position : Vec3
    , color : Vec3
    , normal : Vec3
    , meta :
        { hasColor : Bool
        , hasNormal : Bool
        }
    }


vertex : Vec3 -> Vertex
vertex pos =
    { position = pos
    , color = vec3 1 1 1
    , normal = vec3 0 0 0
    , meta =
        { hasColor = False
        , hasNormal = False
        }
    }


withColor : Vec3 -> Vertex -> Vertex
withColor x ({ meta } as v) =
    { v | color = x, meta = { meta | hasColor = True } }


withNormal : Vec3 -> Vertex -> Vertex
withNormal x ({ meta } as v) =
    { v | normal = x, meta = { meta | hasNormal = True } }
