module Asset exposing (..)


type Obj
    = Ball
    | Tree


objPath : Obj -> String
objPath texture =
    case texture of
        Ball ->
            "obj/Basketball_size6_SF/Basketball_size6_SF.obj"

        Tree ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.obj"


type Texture
    = Empty
    | Placeholder
    | BallDiffuse
    | BallNormal
    | TreeDiffuse


texturePath : Texture -> String
texturePath texture =
    case texture of
        Empty ->
            "img/black.png"

        Placeholder ->
            "img/coords-1.png"

        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"

        BallNormal ->
            "obj/Basketball_size6_SF/Basketball_size6_normal.jpg"

        TreeDiffuse ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.png"
