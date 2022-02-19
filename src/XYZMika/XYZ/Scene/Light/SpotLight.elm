module XYZMika.XYZ.Scene.Light.SpotLight exposing
    ( ShaderData
    , SpotLight
    , color
    , direction
    , fov
    , light
    , position
    , shadowMapData
    , target
    , targetMap
    , toShaderData
    , toVec4
    , withColor
    , withColorVec
    , withIntensity
    , withPosition
    , withShadowMap
    , withTarget
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import XYZMika.Color exposing (Color)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)


type SpotLight
    = SpotLight
        { position : Vec3
        , target : Vec3
        , intensity : Float
        , color : Vec3
        , fov : Float
        , shadowMap : Maybe ShadowMap
        }


type alias ShaderData =
    { light : Vec4
    , direction : Vec3
    , color : Vec3
    , fov : Float
    , resolution : Float
    }


type ShadowMap
    = ShadowMap
        { resolution : Int
        , near : Float
        , far : Float
        }


light : Vec3 -> Float -> SpotLight
light p fov_ =
    SpotLight
        { position = p
        , target = vec3 0 0 0
        , intensity = 1.0
        , color = vec3 1 1 1
        , fov = fov_

        --1.0 - ((fov_ / 180) / pi)
        , shadowMap = Nothing
        }


withShadowMap :
    { resolution : Int
    , near : Float
    , far : Float
    }
    -> SpotLight
    -> SpotLight
withShadowMap x (SpotLight light_) =
    SpotLight
        { light_
            | shadowMap =
                ShadowMap
                    { resolution = x.resolution
                    , near = x.near
                    , far = x.far
                    }
                    |> Just
        }


shadowMapData : SpotLight -> Maybe { resolution : Int, fov : Float, near : Float, far : Float }
shadowMapData (SpotLight spotLight) =
    case spotLight.shadowMap of
        Just (ShadowMap shadowMap) ->
            Just
                { resolution = shadowMap.resolution
                , fov = spotLight.fov
                , near = shadowMap.near
                , far = shadowMap.far
                }

        Nothing ->
            Nothing


withPosition : Vec3 -> SpotLight -> SpotLight
withPosition x (SpotLight light_) =
    SpotLight { light_ | position = x }


withTarget : Vec3 -> SpotLight -> SpotLight
withTarget x (SpotLight light_) =
    SpotLight { light_ | target = x }


withIntensity : Float -> SpotLight -> SpotLight
withIntensity x (SpotLight light_) =
    SpotLight { light_ | intensity = x }


withColorVec : Vec3 -> SpotLight -> SpotLight
withColorVec x (SpotLight light_) =
    SpotLight { light_ | color = x }


withColor : Color -> SpotLight -> SpotLight
withColor color_ (SpotLight light_) =
    SpotLight { light_ | color = XYZMika.Color.toVec3 color_ }


position : SpotLight -> Vec3
position (SpotLight light_) =
    light_.position


color : SpotLight -> Vec3
color (SpotLight light_) =
    light_.color


fov : SpotLight -> Float
fov (SpotLight light_) =
    light_.fov


direction : SpotLight -> Vec3
direction (SpotLight light_) =
    Vec3.direction light_.target light_.position


target : SpotLight -> Vec3
target (SpotLight light_) =
    light_.target


targetMap : (Vec3 -> Vec3) -> SpotLight -> SpotLight
targetMap f (SpotLight light_) =
    SpotLight { light_ | target = f light_.target }


toVec4 : SpotLight -> Vec4
toVec4 (SpotLight light_) =
    Vec4.vec4
        (Vec3.getX light_.position)
        (Vec3.getY light_.position)
        (Vec3.getZ light_.position)
        light_.intensity


toShaderData : Maybe SpotLight -> ShaderData
toShaderData spotLight =
    case spotLight of
        Just (SpotLight light_) ->
            { light = toVec4 (SpotLight light_)
            , direction = direction (SpotLight light_)
            , color = light_.color
            , fov = 1.0 - ((light_.fov / 180) / pi)
            , resolution =
                light_.shadowMap
                    |> Maybe.map (\(ShadowMap x) -> toFloat x.resolution)
                    |> Maybe.withDefault 0.0
            }

        Nothing ->
            { light = vec4 0 0 0 0
            , direction = vec3 0 0 0
            , color = vec3 0 0 0
            , fov = 0.0
            , resolution = 0.0
            }
