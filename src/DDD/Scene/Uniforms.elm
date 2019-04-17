module DDD.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)


type alias Uniforms =
    { rotation : Mat4
    , translate : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , light1 : Vec3
    , light2 : Vec3
    , directionalLight : Vec3
    }