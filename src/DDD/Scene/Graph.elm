module DDD.Scene.Graph exposing (Graph)


type Graph a
    = Graph a
    | Children (List (Graph a))
