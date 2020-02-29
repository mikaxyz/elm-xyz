module XYZMika.Color exposing (..)

import Color exposing (Color)
import Math.Vector3 exposing (Vec3, vec3)


type alias Color =
    Color.Color


toVec3 : Color -> Vec3
toVec3 color =
    Color.toRgba color
        |> (\x -> vec3 x.red x.green x.blue)


grey lightness =
    Color.fromHsla { hue = 0, saturation = 0, lightness = lightness, alpha = 1 }



--


white =
    Color.white


grey50 =
    Color.fromHsla { hue = 0, saturation = 0, lightness = 0.5, alpha = 1 }


eigengrau =
    Color.hsl (22 / 255) (22.0 / 255.0) (29 / 255)



--


red =
    Color.rgb 1 0 0


blue =
    Color.rgb 0 0 1


green =
    Color.rgb 0 1 0


yellow =
    Color.rgb 1 1 0


orange =
    Color.rgb 1 0.5 0.5


purple =
    Color.rgb 0.5 0 0.5


cyan =
    Color.rgb 0 1 1


magenta =
    Color.rgb 1 0 1
