module Scenes.ObjectLoader exposing (addMesh, getObj, init, mesh, sceneOptions)

import Array exposing (Array)
import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object exposing (Object)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Http
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Parser exposing ((|.), (|=), Parser, float, int, spaces, succeed, symbol)
import WebGL exposing (Shader)


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph (DDD.Mesh.Cube.colorful 0.05 0.05 0.05 |> Object.withMesh) [] ]
        , camera = Mat4.makeLookAt (vec3 0 0 2) (vec3 0 0 0) (vec3 0 1 0)
    }


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }


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
                |> Object.withFragmentShader fragmentShader
                |> Object.withPosition (vec3 0 0 0)
    in
    { scene | graph = Graph graphObject [] :: scene.graph }


mesh : String -> List ( Vertex, Vertex, Vertex )
mesh x =
    parse 0.5 x


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 normal;
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;
        uniform vec3 directionalLight;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {

            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);

            highp vec3 ambientLight = vec3(0, 0, 0);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = rotation * vec4(normal, 1.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            vlighting = ambientLight + (directionalLightColor * directional);
            vcolor = color;
            vnormal = normal;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float shade;
        uniform vec3 light1;
        uniform vec3 light2;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;

        void main () {
            gl_FragColor = vec4(vcolor * vlighting, 1.0);
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

        normals : Array Vec3
        normals =
            x
                |> String.lines
                |> List.map (\line -> Parser.run (vector1 "vn") line |> Result.toMaybe)
                |> List.filterMap identity
                |> Array.fromList

        vertMaps : List VertMap
        vertMaps =
            x
                |> String.lines
                |> List.map (\line -> Parser.run vertMapParser line |> Result.toMaybe)
                |> List.filterMap identity

        --                |> Debug.log "vertMaps"
        color =
            vec3 1.0 0.95 0.9

        vertices =
            vertMaps
                |> List.map
                    (\vertMap ->
                        case
                            ( ( Array.get (vertMap.v1 - 1) positions
                              , Array.get (vertMap.v2 - 1) positions
                              , Array.get (vertMap.v3 - 1) positions
                              )
                            , ( Array.get (vertMap.vn1 - 1) normals
                              , Array.get (vertMap.vn2 - 1) normals
                              , Array.get (vertMap.vn3 - 1) normals
                              )
                            )
                        of
                            ( ( Just v1, Just v2, Just v3 ), ( Just vn1, Just vn2, Just vn3 ) ) ->
                                ( Vertex color (v1 |> Vec3.scale scale) vn1
                                , Vertex color (v2 |> Vec3.scale scale) vn2
                                , Vertex color (v3 |> Vec3.scale scale) vn3
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
