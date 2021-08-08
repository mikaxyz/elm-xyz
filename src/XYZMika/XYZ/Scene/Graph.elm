module XYZMika.XYZ.Scene.Graph exposing
    (  Graph(..)
       --, fmap

    , map
    )

import XYZMika.XYZ.Scene.Object exposing (Object)


type Graph object
    = Graph object (List (Graph object))



--map : (object -> object) -> Graph object -> Graph object
--map f (Graph object children) =
--    Graph (f object) (children |> List.map (map f))


map : (Graph object -> Graph object) -> Graph object -> Graph object
map f (Graph object children) =
    f (Graph object (children |> List.map f))



--type Graph attributes object
--    = Graph attributes object
--    | Children (List (Graph attributes object))
