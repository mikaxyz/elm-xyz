module XYZMika.XYZ.AssetStore exposing
    ( Content
    , Store
    , addToStore
    , init
    , loadObj
    , loadTexture
    , mesh
    , texture
    )

import Dict exposing (Dict)
import Http
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import WebGL.Texture
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Parser.Obj


init : (obj -> String) -> (texture -> String) -> Store obj texture
init objPath texturePath =
    Store
        { objPath = objPath
        , texturePath = texturePath
        , assets = Dict.empty
        }


type Asset
    = Mesh (WebGL.Mesh Vertex)
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
        Just (Mesh x) ->
            Just x

        _ ->
            Nothing


addToStore : Content -> Store obj texture -> Store obj texture
addToStore content (Store ({ assets } as store)) =
    let
        ( path, asset ) =
            case content of
                Obj path_ x ->
                    ( path_
                    , XYZMika.XYZ.Parser.Obj.parse
                        { scale = 0.1, color = vec3 1 1 1 }
                        x
                        |> WebGL.triangles
                        |> Mesh
                    )

                Tex path_ result ->
                    ( path_
                    , case result of
                        Ok x ->
                            Texture x

                        Err error ->
                            -- TODO: Dont "swallow" these
                            TextureError error
                    )
    in
    Store { store | assets = assets |> Dict.insert path asset }


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
