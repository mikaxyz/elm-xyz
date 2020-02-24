module XYZMika.XYZ.Scene.Graph exposing (Graph(..))

import XYZMika.XYZ.Scene.Object exposing (Object)


type Graph
    = Graph Object (List Graph)



--type Graph attributes object
--    = Graph attributes object
--    | Children (List (Graph attributes object))
