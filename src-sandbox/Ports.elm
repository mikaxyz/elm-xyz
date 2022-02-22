port module Ports exposing (..)


port onPointerMove : ({ x : Int, y : Int } -> msg) -> Sub msg


port registerToPointerMove : Bool -> Cmd msg
