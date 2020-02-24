module XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL.Texture exposing (Texture)


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , worldMatrix : Mat4

    --
    , directionalLight : Vec3

    --
    , diffuseMap : Texture
    , hasDiffuseMap : Bool
    , normalMap : Texture
    , hasNormalMap : Bool
    , normalMapIntensity : Float
    }
