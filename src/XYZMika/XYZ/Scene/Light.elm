module XYZMika.XYZ.Scene.Light exposing
    ( PointLight
    , color
    , pointLight
    , position
    , toVec4
    , withColor
    , withIntensity
    , withPosition
    , withPositionMap
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)


type PointLight
    = PointLight
        { position : Vec3
        , intensity : Float
        , color : Vec3
        }


pointLight : Vec3 -> PointLight
pointLight p =
    PointLight
        { position = p
        , intensity = 1.0
        , color = vec3 1 1 1
        }


withPosition : Vec3 -> PointLight -> PointLight
withPosition x (PointLight light) =
    PointLight { light | position = x }


withPositionMap : (Vec3 -> Vec3) -> PointLight -> PointLight
withPositionMap f (PointLight light) =
    PointLight { light | position = f light.position }


withIntensity : Float -> PointLight -> PointLight
withIntensity x (PointLight light) =
    PointLight { light | intensity = x }


withColor : Vec3 -> PointLight -> PointLight
withColor x (PointLight light) =
    PointLight { light | color = x }


position : PointLight -> Vec3
position (PointLight light) =
    light.position


color : PointLight -> Vec3
color (PointLight light) =
    light.color


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
