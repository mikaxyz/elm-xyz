module XYZMika.XYZ.Scene.Light exposing
    ( Light
    , directional
    , maybeDirectionalLight
    , maybePointLight
    , pointLight
    , position
    , toHumanReadable
    , withColor
    , withColorVec
    , withIntensity
    , withPosition
    )

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import XYZMika.Color as Color
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight as PointLight exposing (PointLight)


type Light
    = DirectionalLight DirectionalLight
    | PointLight PointLight


pointLight : Vec3 -> Light
pointLight p =
    PointLight (PointLight.light p)


directional : Vec3 -> Light
directional direction =
    DirectionalLight (DirectionalLight.light direction)


withPosition : Vec3 -> Light -> Light
withPosition x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight (light_ |> PointLight.withPosition x)


withIntensity : Float -> Light -> Light
withIntensity x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight (light_ |> PointLight.withIntensity x)


withColor : Color -> Light -> Light
withColor color light =
    withColorVec (Color.toVec3 color) light


withColorVec : Vec3 -> Light -> Light
withColorVec x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight (light_ |> PointLight.withColor x)


position : Light -> Maybe Vec3
position light =
    case light of
        DirectionalLight _ ->
            Nothing

        PointLight light_ ->
            Just (PointLight.position light_)


maybePointLight : Light -> Maybe PointLight
maybePointLight light =
    case light of
        DirectionalLight _ ->
            Nothing

        PointLight x ->
            Just x


maybeDirectionalLight : Light -> Maybe DirectionalLight
maybeDirectionalLight light =
    case light of
        DirectionalLight x ->
            Just x

        PointLight _ ->
            Nothing


toHumanReadable : Light -> String
toHumanReadable light =
    case light of
        DirectionalLight _ ->
            "DirectionalLight"

        PointLight x ->
            "PointLight"
