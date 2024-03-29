module XYZMika.XYZ.Scene.Light exposing
    ( Light
    , directional
    , fromSpotLight
    , maybeDirectionalLight
    , maybePointLight
    , maybeSpotLight
    , pointLight
    , position
    , spotLight
    , spotLightToPointLight
    , targetMap
    , toHumanReadable
    , withColor
    , withColorVec
    , withIntensity
    , withPosition
    , withTarget
    )

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import XYZMika.Color as Color
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight exposing (DirectionalLight)
import XYZMika.XYZ.Scene.Light.PointLight as PointLight exposing (PointLight)
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight exposing (SpotLight)


type Light
    = DirectionalLight DirectionalLight
    | PointLight PointLight
    | SpotLight SpotLight


spotLightToPointLight : Light -> Light
spotLightToPointLight light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight light_

        SpotLight light_ ->
            PointLight.light (SpotLight.position light_)
                |> PointLight.withIntensity (SpotLight.intensity light_)
                |> PointLight.withColor (SpotLight.color light_)
                |> PointLight


fromSpotLight : SpotLight -> Light
fromSpotLight x =
    SpotLight x


spotLight : Vec3 -> Float -> Light
spotLight p fov =
    SpotLight (SpotLight.light p fov)


pointLight : Vec3 -> Light
pointLight p =
    PointLight (PointLight.light p)


directional : Vec3 -> Light
directional direction =
    DirectionalLight (DirectionalLight.light direction)


withTarget : Vec3 -> Light -> Light
withTarget x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight light_

        SpotLight light_ ->
            SpotLight (light_ |> SpotLight.withTarget x)


withPosition : Vec3 -> Light -> Light
withPosition x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight (light_ |> PointLight.withPosition x)

        SpotLight light_ ->
            SpotLight (light_ |> SpotLight.withPosition x)


withIntensity : Float -> Light -> Light
withIntensity x light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight (light_ |> PointLight.withIntensity x)

        SpotLight light_ ->
            SpotLight (light_ |> SpotLight.withIntensity x)


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

        SpotLight light_ ->
            SpotLight (light_ |> SpotLight.withColorVec x)


position : Light -> Maybe Vec3
position light =
    case light of
        DirectionalLight _ ->
            Nothing

        PointLight light_ ->
            Just (PointLight.position light_)

        SpotLight light_ ->
            Just (SpotLight.position light_)


targetMap : (Vec3 -> Vec3) -> Light -> Light
targetMap f light =
    case light of
        DirectionalLight light_ ->
            DirectionalLight light_

        PointLight light_ ->
            PointLight light_

        SpotLight light_ ->
            SpotLight (light_ |> SpotLight.targetMap f)


maybeSpotLight : Light -> Maybe SpotLight
maybeSpotLight light =
    case light of
        DirectionalLight _ ->
            Nothing

        PointLight _ ->
            Nothing

        SpotLight x ->
            Just x


maybePointLight : Light -> Maybe PointLight
maybePointLight light =
    case light of
        DirectionalLight _ ->
            Nothing

        PointLight x ->
            Just x

        SpotLight _ ->
            Nothing


maybeDirectionalLight : Light -> Maybe DirectionalLight
maybeDirectionalLight light =
    case light of
        DirectionalLight x ->
            Just x

        PointLight _ ->
            Nothing

        SpotLight _ ->
            Nothing


toHumanReadable : Light -> String
toHumanReadable light =
    case light of
        DirectionalLight _ ->
            "DirectionalLight"

        PointLight _ ->
            "PointLight"

        SpotLight _ ->
            "SpotLight"
