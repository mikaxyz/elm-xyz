module ShadowMapping.Assets exposing (..)


type TextureId
    = BallDiffuse
    | BrickWall
    | TreeDiffuse


type ObjId
    = Ball
    | Tree


objPath : ObjId -> String
objPath texture =
    case texture of
        Ball ->
            "obj/Basketball_size6_SF/Basketball_size6_SF.obj"

        Tree ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.obj"


texturePath : TextureId -> String
texturePath texture =
    case texture of
        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"

        BrickWall ->
            "img/brickwall-1.jpg"

        TreeDiffuse ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.png"
