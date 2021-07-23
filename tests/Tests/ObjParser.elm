module Tests.ObjParser exposing (suite)

import Expect exposing (Expectation)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Test exposing (..)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Parser.Obj as Obj


color =
    vec3 0.5 1 0.5


suite : Test
suite =
    describe "The Obj Parser"
        [ test "generates tangents/bi tangents for normals" <|
            \_ ->
                let
                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "vt 0 0"
                        , "vt 1 0"
                        , "vt 0 1"
                        , "vn 1 0 0"
                        , "vn 0 1 0"
                        , "vn 0 0 1"
                        , "f 1/1/1 2/2/2 3/3/3"
                        ]
                            |> String.join "\n"

                    --expected =
                    --    [ vec3 0 1 0
                    --    , vec3 0 -1 0
                    --    , vec3 -1 0 0
                    --    ]
                    expected =
                        [ vec3 1 0 0
                        , vec3 1 0 0
                        , vec3 1 0 0
                        ]

                    --expected =
                    --    ( [ Vertex.vertex (vec3 -1 1 0)
                    --            |> Vertex.withUV (vec2 0 0)
                    --            |> Vertex.withNormal (vec3 1 0 0)
                    --            |> Vertex.withTangent (vec3 0 1 0)
                    --            |> Vertex.withColor color
                    --      , Vertex.vertex (vec3 1 1 0)
                    --            |> Vertex.withUV (vec2 1 0)
                    --            |> Vertex.withNormal (vec3 0 1 0)
                    --            |> Vertex.withTangent (vec3 0 -1 0)
                    --            |> Vertex.withColor color
                    --      , Vertex.vertex (vec3 1 -1 0)
                    --            |> Vertex.withUV (vec2 0 1)
                    --            |> Vertex.withNormal (vec3 0 0 1)
                    --            |> Vertex.withTangent (vec3 -1 0 0)
                    --            |> Vertex.withColor color
                    --      ]
                    --    , [ ( 0, 1, 2 ) ]
                    --    )
                    vertices =
                        Obj.parse { scale = 1, color = color } input
                            |> .indexedTrianglesWithTangents

                    subject =
                        vertices
                            |> Tuple.first
                            |> List.map .tangent
                in
                Expect.equal
                    expected
                    subject
        , test "parses triangles with position only" <|
            \_ ->
                let
                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "f 1// 2// 3//"
                        ]
                            |> String.join "\n"

                    expected =
                        [ ( Vertex.vertex (vec3 -1 1 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 1 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 -1 0)
                                |> Vertex.withColor color
                          )
                        ]
                in
                Expect.equal
                    expected
                    (Obj.parse { scale = 1, color = color } input).triangles
        , test "parses triangles with position and uvs" <|
            \_ ->
                let
                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "vt 0 0"
                        , "vt 1 0"
                        , "vt 0 1"
                        , "f 1/1/ 2/2/ 3/3/"
                        ]
                            |> String.join "\n"

                    expected =
                        [ ( Vertex.vertex (vec3 -1 1 0)
                                |> Vertex.withUV (vec2 0 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 1 0)
                                |> Vertex.withUV (vec2 1 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 -1 0)
                                |> Vertex.withUV (vec2 0 1)
                                |> Vertex.withColor color
                          )
                        ]
                in
                Expect.equal
                    expected
                    (Obj.parse { scale = 1, color = color } input).triangles
        , test "parses triangles with position and normals" <|
            \_ ->
                let
                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "vn 1 0 0"
                        , "vn 0 1 0"
                        , "vn 0 0 1"
                        , "f 1//1 2//2 3//3"
                        ]
                            |> String.join "\n"

                    expected =
                        [ ( Vertex.vertex (vec3 -1 1 0)
                                --<<<<<<< HEAD
                                --                                |> Vertex.withColor color
                                --                          , Vertex.vertex (vec3 1 1 0)
                                --                                |> Vertex.withColor color
                                --                          , Vertex.vertex (vec3 1 -1 0)
                                |> Vertex.withNormal (vec3 1 0 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 1 0)
                                |> Vertex.withNormal (vec3 0 1 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 -1 0)
                                |> Vertex.withNormal (vec3 0 0 1)
                                |> Vertex.withColor color
                          )
                        ]
                in
                Expect.equal
                    expected
                    (Obj.parse { scale = 1, color = color } input).triangles
        , skip <|
            --  TODO: handle/triangulate quads
            test "parses faces with 4 vertices"
            <|
                \_ ->
                    let
                        input =
                            [ "v -1.0 1.0 0.0"
                            , "v 1.0 1.0 0.0"
                            , "v 1.0 -1.0 0.0"
                            , "v -1.0 -1.0 0.0"
                            , "f 1 2 3 4"
                            ]
                                |> String.join "\n"

                        expected =
                            [ ( Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              )
                            , ( Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 -1 1 0)
                                    |> Vertex.withColor color
                              )
                            ]

                        --[ ( Vertex color (vec3 -1 1 0) (vec3 0 0 -1)
                        --  , Vertex color (vec3 1 1 0) (vec3 0 0 -1)
                        --  , Vertex color (vec3 1 -1 0) (vec3 0 0 -1)
                        --  )
                        --, ( Vertex color (vec3 1 -1 0) (vec3 0 0 -1)
                        --  , Vertex color (vec3 -1 -1 0) (vec3 0 0 -1)
                        --  , Vertex color (vec3 1 1 0) (vec3 0 0 -1)
                        --  )
                        --]
                        subject =
                            Obj.parse { scale = 1, color = color } input
                    in
                    Expect.equal
                        expected
                        (Obj.parse { scale = 1, color = color } input).triangles
        , test "generates indexed triangles" <|
            \_ ->
                let
                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "vt 0 0"
                        , "vt 1 0"
                        , "vt 0 1"
                        , "vn 1 0 0"
                        , "vn 0 1 0"
                        , "vn 0 0 1"
                        , "f 1/1/1 2/2/2 3/3/3"
                        ]
                            |> String.join "\n"

                    expected =
                        ( [ Vertex.vertex (vec3 -1 1 0)
                                |> Vertex.withUV (vec2 0 0)
                                |> Vertex.withNormal (vec3 1 0 0)
                                |> Vertex.withTangent (vec3 1 0 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 1 0)
                                |> Vertex.withUV (vec2 1 0)
                                |> Vertex.withNormal (vec3 0 1 0)
                                |> Vertex.withTangent (vec3 1 0 0)
                                |> Vertex.withColor color
                          , Vertex.vertex (vec3 1 -1 0)
                                |> Vertex.withUV (vec2 0 1)
                                |> Vertex.withNormal (vec3 0 0 1)
                                |> Vertex.withTangent (vec3 1 0 0)
                                |> Vertex.withColor color
                          ]
                        , [ ( 0, 1, 2 ) ]
                        )

                    subject =
                        Obj.parse { scale = 1, color = color } input
                in
                Expect.equal
                    expected
                    subject.indexedTriangles
        ]
