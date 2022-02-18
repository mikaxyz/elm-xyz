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


graph : Object objectId materialId -> List (Graph (Object objectId materialId)) -> Graph (Object objectId materialId)
graph =
    Tree.tree


shallow : Object objectId materialId -> List (Object objectId materialId) -> Graph (Object objectId materialId)
shallow parent children =
    Tree.tree parent (children |> List.map Tree.singleton)


singleton : Object objectId materialId -> Graph (Object objectId materialId)
singleton =
    Tree.singleton



-- Modification


replaceChildren : List (Graph (Object objectId materialId)) -> Graph (Object objectId materialId) -> Graph (Object objectId materialId)
replaceChildren =
    Tree.replaceChildren


indexedMap : (Int -> a -> b) -> Graph a -> Graph b
indexedMap =
    Tree.indexedMap



-- Folds


root : Graph a -> a
root =
    Tree.label


count : Graph (Object objectId materialId) -> Int
count =
    Tree.count



-- Keep these general since there is usage of Trees of other types:
-- src-examples/View.elm


foldl : (a -> b -> b) -> b -> Graph a -> b
foldl =
    Tree.foldl
