module Scenes.ObjectLoader exposing (addMesh, getObj, init, mesh, sceneOptions)

import Array exposing (Array)
import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import Http
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Parser exposing ((|.), (|=), Parser, float, int, spaces, succeed, symbol)
import WebGL exposing (Shader)


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph (DDD.Mesh.Cube.mesh 0.05 0.05 0.05 |> Object.withMesh) [] ]
        , camera = Mat4.makeLookAt (vec3 0 0 4) (vec3 0 0 0) (vec3 0 1 0)
    }


sceneOptions : Maybe Options
sceneOptions =
    Nothing


getObj : (String -> msg) -> String -> Cmd msg
getObj tagger url =
    Http.get
        { url = url
        , expect = Http.expectString (\x -> tagger (x |> Result.withDefault ""))
        }


addMesh : List ( Vertex, Vertex, Vertex ) -> Scene -> Scene
addMesh tris scene =
    let
        graphObject : Object
        graphObject =
            tris
                |> WebGL.triangles
                |> Object.withMesh
                |> Object.withVertexShader vertexShader
                |> Object.withPosition (vec3 0 0.5 0)
    in
    { scene | graph = Graph graphObject [] :: scene.graph }


mesh : String -> List ( Vertex, Vertex, Vertex )
mesh x =
    parse 0.5 x


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;
        varying vec3 vcolor;

        vec3 campos = camera[3].xyz;

        void main () {
            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
            vcolor = mix(normalize(campos), normalize(position), 0.8);
        }
    |]



-- PARSE


parse : Float -> String -> List ( Vertex, Vertex, Vertex )
parse scale x =
    let
        signedFloat : Parser Float
        signedFloat =
            Parser.oneOf
                [ succeed negate
                    |. symbol "-"
                    |= float
                , float
                ]

        vector1 : String -> Parser Vec3
        vector1 h =
            succeed vec3
                |. symbol h
                |. spaces
                |= signedFloat
                |. spaces
                |= signedFloat
                |. spaces
                |= signedFloat

        positions =
            x
                |> String.lines
                |> List.map (\line -> Parser.run (vector1 "v") line |> Result.toMaybe)
                |> List.filterMap identity
                |> Array.fromList

        normals =
            x
                |> String.lines
                |> List.map (\line -> Parser.run (vector1 "vn") line |> Result.toMaybe)
                |> List.filterMap identity
                |> Array.fromList

        vertMaps =
            x
                |> String.lines
                |> List.map (\line -> Parser.run vertMapParser line |> Result.toMaybe)
                |> List.filterMap identity

        --                |> Debug.log "vertMaps"
        vertices =
            vertMaps
                |> List.map
                    (\vertMap ->
                        case
                            ( Array.get (vertMap.v1 - 1) positions
                            , Array.get (vertMap.v2 - 1) positions
                            , Array.get (vertMap.v3 - 1) positions
                            )
                        of
                            ( Just v1, Just v2, Just v3 ) ->
                                ( Vertex (Color.vec3 Color.red) (v1 |> Vec3.scale scale)
                                , Vertex (Color.vec3 Color.red) (v2 |> Vec3.scale scale)
                                , Vertex (Color.vec3 Color.red) (v3 |> Vec3.scale scale)
                                )
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity
    in
    vertices


type alias VertMap =
    { v1 : Int
    , vt1 : Int
    , vn1 : Int
    , v2 : Int
    , vt2 : Int
    , vn2 : Int
    , v3 : Int
    , vt3 : Int
    , vn3 : Int
    }


vertMapIndex : Parser Int
vertMapIndex =
    Parser.oneOf [ int, Parser.succeed -1 |. symbol "" ]


vertMapParser : Parser VertMap
vertMapParser =
    succeed VertMap
        |. symbol "f"
        |. spaces
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex
        |. spaces
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex
        |. spaces
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex
        |. symbol "/"
        |= vertMapIndex



--toTriangles : Float -> List Vertex -> List ( Vertex, Vertex, Vertex )
--toTriangles scale x =
--    case x of
--        v1 :: v2 :: v3 :: rest ->
--            ( v1 |> Vertex.scale scale, v2 |> Vertex.scale scale, v3 |> Vertex.scale scale ) :: toTriangles scale rest
--
--        rest ->
--            []
--
