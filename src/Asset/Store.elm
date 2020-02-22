module Asset.Store exposing
    ( Content
    , Store
    , addToStore
    , init
    , loadObj
    , loadTexture
    )

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Parser.Obj
import Dict exposing (Dict)
import Http
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import WebGL.Texture


init : Store
init =
    Store { assets = Dict.empty }


type Asset
    = Mesh (WebGL.Mesh Vertex)
    | Texture WebGL.Texture.Texture


type Store
    = Store { assets : Dict String Asset }


type Content
    = Obj String
    | Tex WebGL.Texture.Texture


addToStore : Content -> Store -> Store
addToStore content (Store ({ assets } as store)) =
    let
        asset =
            case content of
                Obj x ->
                    DDD.Parser.Obj.parse
                        { scale = 0.001, color = vec3 1 0.5 0.5 }
                        x
                        |> WebGL.triangles
                        |> Mesh

                Tex x ->
                    Texture x
    in
    Store { store | assets = assets |> Dict.insert "asset" asset }


loadObj : String -> Store -> (Content -> msg) -> Cmd msg
loadObj path store msg =
    Http.get
        { url = path
        , expect =
            Http.expectString
                (\x ->
                    x
                        |> Result.withDefault ""
                        |> Obj
                        |> msg
                )
        }


loadTexture : String -> Store -> (Result WebGL.Texture.Error Content -> msg) -> Cmd msg
loadTexture path store msg =
    Task.attempt
        (\result ->
            result
                |> Result.map Tex
                |> msg
        )
        (WebGL.Texture.load path)



--Http.get
--    { url = path
--    , expect =
--        Http.expectString
--            (\x ->
--                x
--                    |> Result.withDefault ""
--                    |> Obj
--                    |> msg
--            )
--    }
--
--type Obj
--    = Cat
--    | Deer
--    | Wolf
--    | Tree1
--    | Tree2
--
--
--type LoadedAsset
--    = Asset Obj String
--
--
--type alias Store =
--    { cat : Mesh Vertex
--    , deer : Mesh Vertex
--    , wolf : Mesh Vertex
--    , tree1 : Mesh Vertex
--    , tree2 : Mesh Vertex
--    }
--
--
--init : Store
--init =
--    { cat = DDD.Mesh.Cube.gray 1 1 1
--    , deer = DDD.Mesh.Cube.gray 1 1 1
--    , wolf = DDD.Mesh.Cube.gray 1 1 1
--    , tree1 = DDD.Mesh.Cube.gray 1 1 1
--    , tree2 = DDD.Mesh.Cube.gray 1 1 1
--    }
--
--
--addToStore : { asset : Obj, obj : String } -> Store -> Store
--addToStore { asset, obj } store =
--    let
--        m =
--            DDD.Parser.Obj.parse
--                { scale = 0.001, color = vec3 1 0.5 0.5 }
--                obj
--                |> WebGL.triangles
--    in
--    case asset of
--        Cat ->
--            { store | cat = m }
--
--        Deer ->
--            { store | deer = m }
--
--        Wolf ->
--            { store | wolf = m }
--
--        Tree1 ->
--            { store | wolf = m }
--
--        Tree2 ->
--            { store | wolf = m }
--
--
--load : Obj -> String -> ({ asset : Obj, obj : String } -> msg) -> Cmd msg
--load asset path tagger =
--    Http.get
--        { url = path
--        , expect =
--            Http.expectString
--                (\x ->
--                    tagger
--                        { asset = asset
--                        , obj = x |> Result.withDefault ""
--                        }
--                )
--        }
