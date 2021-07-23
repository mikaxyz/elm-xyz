module XYZMika.XYZ.Data.Vertex exposing
    ( Vertex
    , vertex
    , withColor
    , withNormal
    , withTangent
    , withUV
    )

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Vertex =
    { position : Vec3
    , color : Vec3
    , normal : Vec3
    , tangent : Vec3
    , uv : Vec2
    , meta :
        { hasColor : Bool
        , hasNormal : Bool
        , hasTangent : Bool
        , hasUV : Bool
        }
    }


vertex : Vec3 -> Vertex
vertex pos =
    { position = pos
    , color = vec3 1 1 1
    , normal = vec3 0 0 0
    , tangent = vec3 0 0 0
    , uv = vec2 0 0
    , meta =
        { hasColor = False
        , hasNormal = False
        , hasTangent = False
        , hasUV = False
        }
    }


withColor : Vec3 -> Vertex -> Vertex
withColor x ({ meta } as v) =
    { v | color = x, meta = { meta | hasColor = True } }


withNormal : Vec3 -> Vertex -> Vertex
withNormal x ({ meta } as v) =
    { v
        | normal = x |> Vec3.normalize
        , meta = { meta | hasNormal = True }
    }


withTangent : Vec3 -> Vertex -> Vertex
withTangent x ({ meta } as v) =
    { v
        | tangent = Vec3.normalize x
        , meta = { meta | hasTangent = True }
    }


withUV : Vec2 -> Vertex -> Vertex
withUV x ({ meta } as v) =
    { v | uv = x, meta = { meta | hasUV = True } }
