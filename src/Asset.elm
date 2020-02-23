module Asset exposing (..)


type Obj
    = Ball


objPath : Obj -> String
objPath texture =
    case texture of
        Ball ->
            "obj/Basketball_size6_SF/Basketball_size6_SF.obj"


type Texture
    = Placeholder
    | BallDiffuse


texturePath : Texture -> String
texturePath texture =
    case texture of
        Placeholder ->
            "img/coords-1.png"

        BallDiffuse ->
            "obj/Basketball_size6_SF/Basketball_size6.jpg"
