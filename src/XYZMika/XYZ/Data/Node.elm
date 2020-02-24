module XYZMika.XYZ.Data.Node exposing
    ( Node(..)
      --    , flatten
      --    , fold
      --    , treeToList
    )


type Node a
    = Empty
    | Node a (Node a) (Node a)


treeToList : Node a -> List a -> List a
treeToList tree c =
    case tree of
        Empty ->
            c

        Node x left right ->
            x :: treeToList left c ++ treeToList right c


fold : (a -> b -> b) -> b -> Node a -> b
fold f init tree =
    case tree of
        Empty ->
            init

        Node v left right ->
            fold f (f v (fold f init right)) left


flatten : Node a -> List a
flatten =
    fold (::) []
