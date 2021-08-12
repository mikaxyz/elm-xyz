module XYZMika.XYZ.Scene.Camera exposing
    ( Camera
    , init
    , position
    , roll
    , toMat4
    , withOrbitX
    , withOrbitY
    , withPan
    , withPosition
    , withPositionMap
    , withZoom
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


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


roll : Camera -> Float
roll (Camera camera) =
    let
        a =
            Vec3.vec3 0 0 1

        b =
            Vec3.sub
                (Vec3.setY 0 camera.position)
                (Vec3.setY 0 camera.target)
    in
    Vec3.dot a b / (Vec3.length a * Vec3.length b)


withOrbitY : Float -> Camera -> Camera
withOrbitY roll_ (Camera camera) =
    let
        a =
            Vec3.vec3 0 1 0

        m =
            Mat4.makeRotate (roll_ * pi) a
    in
    Camera
        { camera
            | position = Mat4.transform m camera.position

            --, target = Mat4.transform m camera.target
            --, up = Mat4.transform m camera.up
        }


withOrbitX : Float -> Camera -> Camera
withOrbitX roll_ (Camera camera) =
    let
        a =
            Vec3.vec3 1 0 0

        m =
            Mat4.makeRotate (roll_ * pi) a
    in
    Camera
        { camera
            | position = Mat4.transform m camera.position

            --, target = Mat4.transform m camera.target
            --, up = Mat4.transform m camera.up
        }


withPan : Vec2 -> Camera -> Camera
withPan vec (Camera camera) =
    let
        { panX, panY } =
            { panX = Vec2.getX vec
            , panY = Vec2.getY vec
            }

        dir =
            Vec3.direction camera.position camera.target

        horizontal =
            Vec3.cross dir (vec3 0 1 0)

        vertical =
            Vec3.cross dir horizontal |> Vec3.negate

        translate =
            Mat4.makeTranslate (Vec3.scale panX horizontal)
                |> Mat4.translate (Vec3.scale panY vertical)

        m =
            translate
    in
    Camera
        { camera
            | position = Mat4.transform m camera.position
            , target = Mat4.transform m camera.target
        }


withZoom : Float -> Camera -> Camera
withZoom val (Camera camera) =
    let
        dir =
            Vec3.direction camera.position camera.target

        translate =
            dir
                |> Vec3.scale val
                |> Mat4.makeTranslate
    in
    Camera { camera | position = Mat4.transform translate camera.position }


toMat4 : Camera -> Mat4
toMat4 (Camera camera) =
    Mat4.makeLookAt camera.position camera.target camera.up