module ShadowMapping.Assets exposing (..)


type TextureId
    = SneakerDiffuse
    | SneakerNormal


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
        SneakerDiffuse ->
            "obj/used-new-balance-574-classic-free/textures/nb574.jpg"

        SneakerNormal ->
            "obj/used-new-balance-574-classic-free/textures/normals.jpg"
