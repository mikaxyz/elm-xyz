module XYZMika.XYZ.Scene.Light.PointLight exposing
    ( PointLight
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


type PointLight
    = PointLight
        { position : Vec3
        , intensity : Float
        , color : Vec3
        }


light : Vec3 -> PointLight
light p =
    PointLight
        { position = p
        , intensity = 1.0
        , color = vec3 1 1 1
        }


withPosition : Vec3 -> PointLight -> PointLight
withPosition x (PointLight light_) =
    PointLight { light_ | position = x }


withIntensity : Float -> PointLight -> PointLight
withIntensity x (PointLight light_) =
    PointLight { light_ | intensity = x }


withColor : Vec3 -> PointLight -> PointLight
withColor x (PointLight light_) =
    PointLight { light_ | color = x }


position : PointLight -> Vec3
position (PointLight light_) =
    light_.position


color : PointLight -> Vec3
color (PointLight light_) =
    light_.color


type alias Options =
    { lights :
        { directional : Vec3
        , point1 : PointLight
        , point2 : PointLight
        }
    }


toVec4 : PointLight -> Vec4
toVec4 (PointLight light_) =
    Vec4.vec4
        (Vec3.getX light_.position)
        (Vec3.getY light_.position)
        (Vec3.getZ light_.position)
        light_.intensity
