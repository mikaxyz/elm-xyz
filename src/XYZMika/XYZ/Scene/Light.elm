module XYZMika.XYZ.Scene.Light exposing (PointLight, toVec4)

import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)


type alias PointLight =
    { position : Vec3
    , intensity : Float
    }


type alias Options =
    { lights :
        { directional : Vec3
        , point1 : PointLight
        , point2 : PointLight
        }
    }


toVec4 : PointLight -> Vec4
toVec4 light =
    Vec4.vec4
        (Vec3.getX light.position)
        (Vec3.getY light.position)
        (Vec3.getZ light.position)
        light.intensity
