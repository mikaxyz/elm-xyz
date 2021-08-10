module XYZMika.XYZ.Scene.Graph exposing
    ( Graph(..)
    , map
    , toList
    , traverse
    )


type Graph object
    = Graph object (List (Graph object))


map : (a -> b) -> Graph a -> Graph b
map f (Graph object children) =
    Graph (f object) (children |> List.map (map f))


traverse : (Graph a -> Graph a) -> Graph a -> Graph a
traverse f (Graph object children) =
    f (Graph object (children |> List.map f))


toList : Graph a -> List a
toList graph =
    case graph of
        Graph a [] ->
            [ a ]

        Graph a children ->
            a :: (children |> List.concatMap toList)
