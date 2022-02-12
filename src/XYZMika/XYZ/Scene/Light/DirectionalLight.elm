module XYZMika.XYZ.Scene.Light.DirectionalLight exposing
    ( DirectionalLight
    , color
    , light
    , toVec4
    , withColor
    , withIntensity
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)


type DirectionalLight
    = DirectionalLight
        { direction : Vec3
        , intensity : Float
        , color : Vec3
        }


light : Vec3 -> DirectionalLight
light direction =
    DirectionalLight
        { direction = direction
        , intensity = 1.0
        , color = vec3 1 1 1
        }


withDirection : Vec3 -> DirectionalLight -> DirectionalLight
withDirection x (DirectionalLight light_) =
    DirectionalLight { light_ | direction = x }


withIntensity : Float -> DirectionalLight -> DirectionalLight
withIntensity x (DirectionalLight light_) =
    DirectionalLight { light_ | intensity = x }


withColor : Vec3 -> DirectionalLight -> DirectionalLight
withColor x (DirectionalLight light_) =
    DirectionalLight { light_ | color = x }


color : DirectionalLight -> Vec3
color (DirectionalLight light_) =
    light_.color


toVec4 : DirectionalLight -> Vec4
toVec4 (DirectionalLight light_) =
    Vec4.vec4
        (Vec3.getX light_.direction)
        (Vec3.getY light_.direction)
        (Vec3.getZ light_.direction)
        light_.intensity
