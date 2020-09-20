module Tests.ObjParser exposing (suite)

import Expect exposing (Expectation)
import Math.Vector3 exposing (vec3)
import Test exposing (..)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Parser.Obj as Obj


suite : Test
suite =
    describe "The Obj Parser"
        [ test "parses geometry for faces with 3 vertices" <|
            \_ ->
                let
                    color =
                        vec3 0.5 1 0.5

                    input =
                        [ "v -1.0 1.0 0.0"
                        , "v 1.0 1.0 0.0"
                        , "v 1.0 -1.0 0.0"
                        , "f 1 2 3"
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
        , skip <|
            --  TODO: handle/triangulate quads
            test "parses faces with 4 vertices"
            <|
                \_ ->
                    let
                        color =
                            vec3 0.5 1 0.5

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
                              , Vertex.vertex (vec3 1 1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 1 -1 0)
                                    |> Vertex.withColor color
                              )
                            , ( Vertex.vertex (vec3 1 -1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 -1 -1 0)
                                    |> Vertex.withColor color
                              , Vertex.vertex (vec3 1 1 0)
                                    |> Vertex.withColor color
                              )
                            ]
                    in
                    Expect.equal
                        expected
                        (Obj.parse { scale = 1, color = color } input).triangles
        ]
