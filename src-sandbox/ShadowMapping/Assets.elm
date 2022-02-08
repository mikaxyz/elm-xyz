module ShadowMapping.Assets exposing (..)


type TextureId
    = BallDiffuse
    | BrickWall
    | TreeDiffuse


type ObjId
    = SneakerXyz


objPath : ObjId -> String
objPath obj =
    case obj of
        SneakerXyz ->
            "obj/sneaker.xyz"


texturePath : TextureId -> String
texturePath texture =
    case texture of
        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"

        BrickWall ->
            "img/brickwall-1.jpg"

        TreeDiffuse ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.png"
