module XYZMika.XYZ.Scene.Graph exposing (Graph, graph)

import Tree exposing (Tree)
import XYZMika.XYZ.Scene.Object exposing (Object)


type alias Graph a =
    Tree a


graph : Object materialId -> List (Object materialId) -> Graph (Object materialId)
graph parent children =
    Tree.tree parent (children |> List.map Tree.singleton)
