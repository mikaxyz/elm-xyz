module XYZMika.XYZ.AssetStore exposing
    ( Content
    , Store
    , addMeshToStore
    , addToStore
    , init
    , loadObj
    , loadTexture
    , mesh
    , texture
    , vertices
    , verticesIndexed
    )

import Dict exposing (Dict)
import Http
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import WebGL.Texture
import XYZMika.Debug as Dbug
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Parser.Obj


init : (obj -> String) -> (texture -> String) -> Store obj texture
init objPath texturePath =
    Store
        { objPath = objPath
        , texturePath = texturePath
        , assets = Dict.empty
        }


type Asset
    = Mesh (List ( Vertex, Vertex, Vertex )) ( List Vertex, List ( Int, Int, Int ) ) (WebGL.Mesh Vertex)
    | Texture WebGL.Texture.Texture
    | TextureError WebGL.Texture.Error


type Store obj texture
    = Store
        { objPath : obj -> String
        , texturePath : texture -> String
        , assets : Dict String Asset
        }


type Content
    = Obj String String
    | Tex String (Result WebGL.Texture.Error WebGL.Texture.Texture)


texture : texture -> Store obj texture -> Maybe WebGL.Texture.Texture
texture texture_ (Store { texturePath, assets }) =
    case assets |> Dict.get (texturePath texture_) of
        Just (Texture x) ->
            Just x

        _ ->
            Nothing


mesh : obj -> Store obj texture -> Maybe (WebGL.Mesh Vertex)
mesh obj (Store { objPath, assets }) =
    case assets |> Dict.get (objPath obj) of
        Just (Mesh v iv x) ->
            Just x

        _ ->
            Nothing


vertices : obj -> Store obj texture -> Maybe (List ( Vertex, Vertex, Vertex ))
vertices obj (Store { objPath, assets }) =
    case assets |> Dict.get (objPath obj) of
        Just (Mesh v iv x) ->
            Just v

        _ ->
            Nothing


verticesIndexed : obj -> Store obj texture -> Maybe ( List Vertex, List ( Int, Int, Int ) )
verticesIndexed obj (Store { objPath, assets }) =
    case assets |> Dict.get (objPath obj) of
        Just (Mesh _ iv _) ->
            Just iv

        _ ->
            Nothing


addToStore : Float -> Content -> Store obj texture -> Store obj texture
addToStore scale content (Store ({ assets } as store)) =
    let
        ( path, asset ) =
            case content of
                Obj path_ x ->
                    ( path_
                    , XYZMika.XYZ.Parser.Obj.parse { scale = scale, color = vec3 1 1 1 } x
                        |> (\{ triangles, indexedTriangles } -> Mesh triangles indexedTriangles (WebGL.triangles triangles))
                    )

                Tex path_ result ->
                    ( path_
                    , case result of
                        Ok x ->
                            Texture x

                        Err error ->
                            -- TODO: Dont "swallow" these
                            TextureError (Dbug.todo "error" error)
                    )
    in
    Store { store | assets = assets |> Dict.insert path asset }


addMeshToStore : obj -> WebGL.Mesh Vertex -> Store obj texture -> Store obj texture
addMeshToStore obj mesh_ (Store ({ objPath, assets } as store)) =
    Store { store | assets = assets |> Dict.insert (objPath obj) (Mesh [] ( [], [] ) mesh_) }


loadObj : obj -> Store obj texture -> (Content -> msg) -> Cmd msg
loadObj obj (Store { objPath }) msg =
    Http.get
        { url = objPath obj
        , expect =
            Http.expectString
                (\x ->
                    x
                        |> Result.withDefault ""
                        |> Obj (objPath obj)
                        |> msg
                )
        }


loadTexture : texture -> Store obj texture -> (Content -> msg) -> Cmd msg
loadTexture texture_ (Store { texturePath }) msg =
    Task.attempt
        (\result ->
            Tex (texturePath texture_) result
                |> msg
        )
        (WebGL.Texture.load (texturePath texture_))
