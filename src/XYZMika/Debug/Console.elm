module XYZMika.Debug exposing (..)


log : String -> a -> a
log =
    Debug.log


todo : String -> a -> a
todo str x =
    Debug.log ("ERROR " ++ str) x
        |> Debug.todo str
