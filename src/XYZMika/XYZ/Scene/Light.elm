module XYZMika.XYZ.Scene.Light exposing
    ( PointLight
    , pointLight
    , position
    , toVec4
    , withIntensity
    )

import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)


type PointLight
    = PointLight
        { position : Vec3
        , intensity : Float
        }


pointLight : Vec3 -> PointLight
pointLight p =
    PointLight
        { position = p
        , intensity = 1.0
        }


withIntensity : Float -> PointLight -> PointLight
withIntensity x (PointLight light) =
    PointLight { light | intensity = x }


position : PointLight -> Vec3
position (PointLight light) =
    light.position


type alias Options =
    { lights :
        { directional : Vec3
        , point1 : PointLight
        , point2 : PointLight
        }
    }


toVec4 : PointLight -> Vec4
toVec4 (PointLight light) =
    Vec4.vec4
        (Vec3.getX light.position)
        (Vec3.getY light.position)
        (Vec3.getZ light.position)
        light.intensity
