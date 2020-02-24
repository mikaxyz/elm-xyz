module DDD.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL.Texture exposing (Texture)


type alias Uniforms =
    { rotation : Mat4
    , translate : Mat4
    , perspective : Mat4
    , camera : Mat4
    , directionalLight : Vec3
    , worldMatrix : Mat4
    , texture : Texture
    , hasTextureMap : Bool
    , normalMap : Texture
    , hasNormalMap : Bool
    , normalMapIntensity : Float
    }
