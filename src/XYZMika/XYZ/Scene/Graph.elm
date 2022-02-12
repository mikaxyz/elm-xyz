module XYZMika.XYZ.Scene.Graph exposing
    ( Graph, graph, singleton, shallow
    , indexedMap, replaceChildren
    , root, count, foldl
    )

{-| Scene Object Graph


# Creation

@docs Graph, graph, singleton, shallow


# Modification

@docs indexedMap, replaceChildren


# Folds

@docs root, count, foldl

-}

import Tree
import XYZMika.XYZ.Scene.Object exposing (Object)



-- Creation


type alias Graph a =
    Tree.Tree a


graph : Object materialId -> List (Graph (Object materialId)) -> Graph (Object materialId)
graph =
    Tree.tree


shallow : Object materialId -> List (Object materialId) -> Graph (Object materialId)
shallow parent children =
    Tree.tree parent (children |> List.map Tree.singleton)


singleton : Object materialId -> Graph (Object materialId)
singleton =
    Tree.singleton



-- Modification


replaceChildren : List (Graph (Object materialId)) -> Graph (Object materialId) -> Graph (Object materialId)
replaceChildren =
    Tree.replaceChildren


indexedMap : (Int -> a -> b) -> Graph a -> Graph b
indexedMap =
    Tree.indexedMap



-- Folds


root : Graph a -> a
root =
    Tree.label


count : Graph (Object materialId) -> Int
count =
    Tree.count



-- Keep these general since there is usage of Trees of other types:
-- src-examples/View.elm


foldl : (a -> b -> b) -> b -> Graph a -> b
foldl =
    Tree.foldl
