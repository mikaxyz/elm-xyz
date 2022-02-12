module XYZMika.XYZ.Scene.Light.SpotLight exposing
    ( SpotLight
    , color
    , light
    , position
    , toVec4
    , withColor
    , withIntensity
    , withPosition
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)


type SpotLight
    = SpotLight
        { position : Vec3
        , target : Vec3
        , intensity : Float
        , color : Vec3
        }


light : Vec3 -> SpotLight
light p =
    SpotLight
        { position = p
        , target = vec3 0 0 0
        , intensity = 1.0
        , color = vec3 1 1 1
        }


withPosition : Vec3 -> SpotLight -> SpotLight
withPosition x (SpotLight light_) =
    SpotLight { light_ | position = x }


withIntensity : Float -> SpotLight -> SpotLight
withIntensity x (SpotLight light_) =
    SpotLight { light_ | intensity = x }


withColor : Vec3 -> SpotLight -> SpotLight
withColor x (SpotLight light_) =
    SpotLight { light_ | color = x }


position : SpotLight -> Vec3
position (SpotLight light_) =
    light_.position


color : SpotLight -> Vec3
color (SpotLight light_) =
    light_.color


toVec4 : SpotLight -> Vec4
toVec4 (SpotLight light_) =
    Vec4.vec4
        (Vec3.getX light_.position)
        (Vec3.getY light_.position)
        (Vec3.getZ light_.position)
        light_.intensity
