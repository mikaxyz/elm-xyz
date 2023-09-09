module XYZMika.WebGL exposing
    ( FrameBuffer
    , frameBuffer
    , supportsFrameBuffers
    , toHtmlWithFrameBuffers
    )

import Html exposing (Attribute, Html)
import WebGL exposing (Entity, Option)
import WebGL.Texture exposing (Texture)


supportsFrameBuffers : (() -> a) -> (() -> a) -> a
supportsFrameBuffers f default =
    default ()


type FrameBuffer
    = FrameBuffer


frameBuffer : ( Int, Int ) -> List Entity -> FrameBuffer
frameBuffer _ _ =
    FrameBuffer


toHtmlWithFrameBuffers :
    List FrameBuffer
    -> List Option
    -> List (Attribute msg)
    -> (List Texture -> List Entity)
    -> Html msg
toHtmlWithFrameBuffers _ _ _ _ =
    Html.code [] [ Html.text "Tried to display WebGL view with wrong library" ]
