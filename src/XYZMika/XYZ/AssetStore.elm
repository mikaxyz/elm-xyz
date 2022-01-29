module XYZMika.XYZ.AssetStore exposing
    ( Content
    , Error(..)
    , Store
    , addToStore
    , init
    , loadObj
    , loadObjWithScale
    , loadTexture
    , loadXyz
    , mesh
    , texture
    , vertices
    , verticesIndexed
    )

import Dict exposing (Dict)
import Http
import Json.Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import WebGL.Texture
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Parser.Obj
import XYZMika.XYZ.Parser.Serialize


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


type Store obj texture
    = Store
        { objPath : obj -> String
        , texturePath : texture -> String
        , assets : Dict String Asset
        }


type Error
    = HttpError Http.Error
    | XyzSerializeError Json.Decode.Error
    | TextureLoadError WebGL.Texture.Error


type alias Triangles =
    { triangles : List ( Vertex, Vertex, Vertex )
    , indexedTriangles : ( List Vertex, List ( Int, Int, Int ) )
    }


type Content
    = Obj String Triangles
    | Tex String WebGL.Texture.Texture


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
        Just (Mesh _ _ x) ->
            Just x

        _ ->
            Nothing


vertices : obj -> Store obj texture -> Maybe (List ( Vertex, Vertex, Vertex ))
vertices obj (Store { objPath, assets }) =
    case assets |> Dict.get (objPath obj) of
        Just (Mesh v _ _) ->
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


addToStore : Content -> Store obj texture -> Store obj texture
addToStore content (Store ({ assets } as store)) =
    let
        ( path, asset ) =
            case content of
                Obj path_ { triangles, indexedTriangles } ->
                    ( path_
                    , Mesh triangles indexedTriangles (WebGL.triangles triangles)
                    )

                Tex path_ texture_ ->
                    ( path_
                    , Texture texture_
                    )
    in
    Store { store | assets = assets |> Dict.insert path asset }


loadObj : obj -> Store obj texture -> (Result Error Content -> msg) -> Cmd msg
loadObj =
    loadObjWithConfig { scale = 1, transform = Mat4.identity }


loadObjWithScale : Float -> obj -> Store obj texture -> (Result Error Content -> msg) -> Cmd msg
loadObjWithScale scale =
    loadObjWithConfig { scale = scale, transform = Mat4.identity }


loadObjWithConfig :
    { scale : Float
    , transform : Mat4
    }
    -> obj
    -> Store obj texture
    -> (Result Error Content -> msg)
    -> Cmd msg
loadObjWithConfig config obj (Store { objPath }) msg =
    Http.get
        { url = objPath obj
        , expect =
            Http.expectString
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen
                            (\objFile ->
                                XYZMika.XYZ.Parser.Obj.parse
                                    { transform = config.transform
                                    , scale = config.scale
                                    , color = vec3 1 1 1
                                    }
                                    objFile
                                    |> Obj (objPath obj)
                                    |> Ok
                            )
                        |> msg
                )
        }


loadXyz : obj -> Store obj texture -> (Result Error Content -> msg) -> Cmd msg
loadXyz obj (Store { objPath }) msg =
    Http.get
        { url = objPath obj
        , expect =
            Http.expectString
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen
                            (\json ->
                                XYZMika.XYZ.Parser.Serialize.decode json
                                    |> Result.map (Obj (objPath obj))
                                    |> Result.mapError XyzSerializeError
                            )
                        |> msg
                )
        }


loadTexture : texture -> Store obj texture -> (Result Error Content -> msg) -> Cmd msg
loadTexture texture_ (Store { texturePath }) msg =
    Task.attempt
        (\result ->
            result
                |> Result.map (Tex (texturePath texture_))
                |> Result.mapError TextureLoadError
                |> msg
        )
        (WebGL.Texture.load (texturePath texture_))
