module DDD.Scene.Graph exposing (Graph)


type Graph attributes object
    = Graph attributes object
    | Children (List (Graph attributes object))
