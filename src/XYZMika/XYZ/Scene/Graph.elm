module XYZMika.XYZ.Scene.Graph exposing
    ( Graph
    , init
    , map
    , mapChildren
    , mapRoot
    , toList
    , traverse
    , unwrap
    , unwrapChildren
    , withChildren
    )


type Graph object
    = Graph object (List (Graph object))


init : a -> Graph a
init object =
    Graph object []


withChildren : List (Graph object) -> Graph object -> Graph object
withChildren x (Graph object _) =
    Graph object x


mapChildren : (List (Graph a) -> List (Graph a)) -> Graph a -> Graph a
mapChildren f (Graph object children) =
    Graph object (f children)


unwrap : Graph a -> a
unwrap (Graph object _) =
    object


unwrapChildren : Graph a -> List (Graph a)
unwrapChildren (Graph _ children) =
    children


mapRoot : (a -> a) -> Graph a -> Graph a
mapRoot f (Graph object children) =
    Graph (f object) children


map : (a -> b) -> Graph a -> Graph b
map f (Graph object children) =
    Graph (f object) (children |> List.map (map f))


traverse : (Graph a -> Graph a) -> Graph a -> Graph a
traverse f (Graph object children) =
    Graph object (children |> List.map (\g -> traverse f g))
        |> f


toList : Graph a -> List a
toList graph =
    case graph of
        Graph a [] ->
            [ a ]

        Graph a children ->
            a :: (children |> List.concatMap toList)
