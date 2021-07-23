module XYZMika.XYZ.Scene.Graph exposing (Graph(..), map)

import XYZMika.XYZ.Scene.Object exposing (Object)


type Graph materialId
    = Graph (Object materialId) (List (Graph materialId))


map : (List (Graph materialId) -> List (Graph materialId)) -> Graph materialId -> Graph materialId
map f (Graph object children) =
    Graph object (f children)



--type Graph attributes object
--    = Graph attributes object
--    | Children (List (Graph attributes object))
