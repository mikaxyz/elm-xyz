module DDD.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)


type alias Uniforms =
    { rotation : Mat4
    , translate : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }
