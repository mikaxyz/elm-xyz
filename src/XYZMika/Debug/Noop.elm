module XYZMika.Debug exposing (..)


log : String -> a -> a
log =
    always identity


todo : String -> a -> a
todo =
    always identity
