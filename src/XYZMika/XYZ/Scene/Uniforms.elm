module XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)


type alias Uniforms u =
    { u
        | perspective : Mat4
        , camera : Mat4
        , worldMatrix : Mat4
        , uColor : Vec3
    }
