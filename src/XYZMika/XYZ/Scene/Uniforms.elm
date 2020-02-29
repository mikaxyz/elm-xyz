module XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)

import Math.Matrix4 exposing (Mat4)


type alias Uniforms u =
    { u
        | sceneCamera : Mat4
        , scenePerspective : Mat4
        , sceneMatrix : Mat4
    }
