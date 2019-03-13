module DDD.Scene.Graph exposing (Graph(..))

import DDD.Scene.Object exposing (Object)


type Graph
    = Graph Object (List Graph)



--type Graph attributes object
--    = Graph attributes object
--    | Children (List (Graph attributes object))
