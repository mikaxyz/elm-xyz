module XYZMika.XYZ.Scene.Camera exposing (Camera, init, position, toMat4, withPosition, withPositionMap)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)


type Camera
    = Camera
        { position : Vec3
        , target : Vec3
        , up : Vec3
        }


init position_ target_ =
    Camera
        { position = position_
        , target = target_
        , up = vec3 0 1 0
        }


position : Camera -> Vec3
position (Camera camera) =
    camera.position


withPosition : Vec3 -> Camera -> Camera
withPosition x (Camera camera) =
    Camera { camera | position = x }


withPositionMap : (Vec3 -> Vec3) -> Camera -> Camera
withPositionMap f (Camera camera) =
    Camera { camera | position = f camera.position }


toMat4 : Camera -> Mat4
toMat4 (Camera camera) =
    Mat4.makeLookAt camera.position camera.target camera.up
