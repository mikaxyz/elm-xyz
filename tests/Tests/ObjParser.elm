module Tests.ObjParser exposing (suite)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Parser.Obj as Obj
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Math.Vector3 exposing (vec3)
import Parser
import Test exposing (..)


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
                        [ ( Vertex color (vec3 -1 1 0) (vec3 0 0 -1)
                          , Vertex color (vec3 1 1 0) (vec3 0 0 -1)
                          , Vertex color (vec3 1 -1 0) (vec3 0 0 -1)
                          )
                        ]
                in
                Expect.equal
                    expected
                    (Obj.parse { scale = 1, color = color } input)
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
                            [ ( Vertex color (vec3 -1 1 0) (vec3 0 0 -1)
                              , Vertex color (vec3 1 1 0) (vec3 0 0 -1)
                              , Vertex color (vec3 1 -1 0) (vec3 0 0 -1)
                              )
                            , ( Vertex color (vec3 1 -1 0) (vec3 0 0 -1)
                              , Vertex color (vec3 -1 -1 0) (vec3 0 0 -1)
                              , Vertex color (vec3 1 1 0) (vec3 0 0 -1)
                              )
                            ]
                    in
                    Expect.equal
                        expected
                        (Obj.parse { scale = 1, color = color } input)
        ]
