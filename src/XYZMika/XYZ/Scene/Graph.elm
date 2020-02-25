module XYZMika.XYZ.Scene.Graph exposing (Graph(..))

import XYZMika.XYZ.Scene.Object exposing (Object)


type Graph materialId
    = Graph (Object materialId) (List (Graph materialId))



--type Graph attributes object
--    = Graph attributes object
--    | Children (List (Graph attributes object))
