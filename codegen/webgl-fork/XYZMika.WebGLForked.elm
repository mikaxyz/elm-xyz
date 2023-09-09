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
    f ()


type alias FrameBuffer =
    WebGL.FrameBuffer


frameBuffer : ( Int, Int ) -> List Entity -> FrameBuffer
frameBuffer =
    WebGL.frameBuffer


toHtmlWithFrameBuffers :
    List WebGL.FrameBuffer
    -> List Option
    -> List (Attribute msg)
    -> (List Texture -> List Entity)
    -> Html msg
toHtmlWithFrameBuffers =
    WebGL.toHtmlWithFrameBuffers
