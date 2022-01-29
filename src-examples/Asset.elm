module Asset exposing (..)


type Obj
    = Ball
    | Tree
    | Cube
    | UvCube
    | Monkey
    | Deer
    | Wolf
    | Cat
    | Sneaker
    | SneakerXyz


objPath : Obj -> String
objPath texture =
    case texture of
        Ball ->
            "obj/Basketball_size6_SF/Basketball_size6_SF.obj"

        Tree ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.obj"

        Cube ->
            "obj/cube.obj"

        UvCube ->
            "obj/uvcube/uvcube.obj"

        Deer ->
            "obj/deer.obj"

        Monkey ->
            "obj/monkey.obj"

        Wolf ->
            "obj/wolf.obj"

        Cat ->
            "obj/cat.obj"

        Sneaker ->
            "obj/used-new-balance-574-classic-free/nb574.obj"

        SneakerXyz ->
            "obj/sneaker.xyz"


type Texture
    = MissingFile
    | Placeholder
    | BallDiffuse
    | BallNormal
    | TreeDiffuse
    | UvCubeDiffuse
    | BrickWallDiffuse
    | BrickWallNormal
    | SneakerDiffuse
    | SneakerNormal


texturePath : Texture -> String
texturePath texture =
    case texture of
        MissingFile ->
            "img/missing.png"

        Placeholder ->
            "img/coords-1.png"

        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"

        BallNormal ->
            "obj/Basketball_size6_SF/Basketball_size6_normal.jpg"

        TreeDiffuse ->
            "obj/fat_bottomed_tree/fat_bottomed_tree.png"

        UvCubeDiffuse ->
            "obj/uvcube/uvcube.png"

        BrickWallDiffuse ->
            "obj/Brick_Wall_017_SD/Brick_Wall_017_basecolor.jpg"

        BrickWallNormal ->
            "obj/Brick_Wall_017_SD/Brick_Wall_017_normal.jpg"

        SneakerDiffuse ->
            "obj/used-new-balance-574-classic-free/textures/nb574.jpg"

        SneakerNormal ->
            "obj/used-new-balance-574-classic-free/textures/normals.jpg"
