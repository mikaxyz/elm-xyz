module XYZMika.XYZ.Parser.Obj exposing (parse, parseIndexed)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.Color as Color
import XYZMika.XYZ.Data.Vertex as Vertex


type alias Index =
    Int



--
--type V
--    = V Vec3
--
--
--type T
--    = T Vec2
--
--
--type N
--    = N Vec3


type VertexIndices
    = Position Index
    | PositionUv Index Index
    | PositionNormal Index Index
    | PositionUvNormal Index Index Index


type Vertex
    = V Vec3



--type Triangle
--    = Triangle ( Vertex, Vertex, Vertex )
--type alias Obj =
--    { triangles : Dict Index Triangle
--    }


type alias Options =
    { scale : Float
    , color : Vec3
    }


type ObjData
    = Geometry Vec3
    | Texture Vec2
    | Normal Vec3
    | Vmap3 VertMap VertMap VertMap
    | Ignored String
    | Error String



--type alias VertMap3 =
--    { m1 : VertMap, m2 : VertMap, m3 : VertMap }


type alias VertMap =
    { v : Int
    , vt : Maybe Int
    , vn : Maybe Int
    }


parseIndexed : Options -> String -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
parseIndexed options input =
    String.lines input
        |> List.map parseLine
        --|> List.map (Debug.log "line")
        |> toIndexedData
        --|> List.map (Debug.log "data")
        |> always ( [], [] )


toIndexedData : List ObjData -> List ( Vertex, Vertex, Vertex )
toIndexedData data =
    let
        --toVertex { v, vt, vn } =
        --    Vertex v vt vn
        isVmap x =
            case x of
                Vmap3 _ _ _ ->
                    True

                _ ->
                    False

        toVertex : ( VertMap, VertMap, VertMap ) -> ( Vertex, Vertex, Vertex )
        toVertex x =
            ( V Vec3.i, V Vec3.i, V Vec3.i )

        toVertmap : ObjData -> Maybe ( VertMap, VertMap, VertMap )
        toVertmap x =
            case x of
                Vmap3 v1 v2 v3 ->
                    Just ( v1, v2, v3 )

                _ ->
                    Nothing
    in
    data
        |> List.filterMap toVertmap
        |> List.map (Debug.log "data")
        |> List.map toVertex



--case data of
--    Vmap3 v1 v2 v3 ->
--        ( toVertex v1, toVertex v2, toVertex v3 )


parseLine : String -> ObjData
parseLine line =
    let
        parseVector : String -> Vec3
        parseVector str =
            str
                |> String.dropLeft 2
                |> String.trim
                |> String.split " "
                |> List.filterMap String.toFloat
                |> (\x ->
                        case x of
                            c1 :: c2 :: c3 :: _ ->
                                vec3 c1 c2 c3

                            _ ->
                                vec3 0 0 0
                   )

        parseVector2 : String -> Vec2
        parseVector2 str =
            str
                |> String.dropLeft 2
                |> String.trim
                |> String.split " "
                |> List.filterMap String.toFloat
                |> (\x ->
                        case x of
                            c1 :: c2 :: _ ->
                                vec2 c1 c2

                            _ ->
                                vec2 0 0
                   )

        zeroIndexVmap : VertMap -> VertMap
        zeroIndexVmap { v, vt, vn } =
            VertMap (v - 1)
                (vt |> Maybe.map (\x -> x - 1))
                (vn |> Maybe.map (\x -> x - 1))

        --parseVmaps : String -> VertMap3
        --parseVmaps str =
        --    let
        --        -- TOOD: Handle quads?
        --        vmaps =
        --            str
        --                |> String.dropLeft 2
        --                |> String.trim
        --                |> String.split " "
        --                |> List.map
        --                    (\vm ->
        --                        let
        --                            v =
        --                                vm
        --                                    |> String.split "/"
        --                                    |> List.map String.toInt
        --                        in
        --                        case v of
        --                            c1 :: c2 :: c3 :: _ ->
        --                                VertMap (c1 |> Maybe.withDefault 0) c2 c3
        --
        --                            c1 :: c2 :: _ ->
        --                                VertMap (c1 |> Maybe.withDefault 0) c2 Nothing
        --
        --                            c1 :: _ ->
        --                                VertMap (c1 |> Maybe.withDefault 0) Nothing Nothing
        --
        --                            _ ->
        --                                -- TOOD: Add to errors instead
        --                                VertMap 0 Nothing Nothing
        --                    )
        --                |> List.map zeroIndexVmap
        --    in
        --    case vmaps of
        --        v1 :: v2 :: v3 :: _ ->
        --            VertMap3 v1 v2 v3
        --
        --        _ ->
        --            -- TOOD: Add to errors instead
        --            VertMap3
        --                (VertMap 0 Nothing Nothing)
        --                (VertMap 0 Nothing Nothing)
        --                (VertMap 0 Nothing Nothing)
        parseVertMap : String -> Maybe VertMap
        parseVertMap x =
            case String.split "/" x |> List.map String.toInt of
                v :: t :: n :: [] ->
                    v
                        |> Maybe.map (\v_ -> VertMap v_ t n)

                _ ->
                    Nothing

        --|> Maybe.withDefault
        parseVertMaps : String -> Maybe ( VertMap, VertMap, VertMap )
        parseVertMaps x =
            case x |> String.split " " of
                m1 :: m2 :: m3 :: [] ->
                    Maybe.map3
                        (\vm1 vm2 vm3 -> ( vm1, vm2, vm3 ))
                        (parseVertMap m1)
                        (parseVertMap m2)
                        (parseVertMap m3)

                _ ->
                    Nothing
    in
    case String.toList line of
        'v' :: ' ' :: _ ->
            Geometry (parseVector line)

        'v' :: 't' :: ' ' :: _ ->
            Texture (parseVector2 line)

        'v' :: 'n' :: ' ' :: _ ->
            Normal (parseVector line)

        --'f' :: ' ' :: v1 :: '/' :: t1 :: '/' :: n1 :: '/' :: ' ' :: v2 :: '/' :: t2 :: '/' :: n2 :: '/' :: ' ' :: v3 :: '/' :: t3 :: '/' :: n3 :: '/' :: ' ' ->
        'f' :: ' ' :: data ->
            parseVertMaps (String.fromList data)
                |> Maybe.map (\( m1, m2, m3 ) -> Vmap3 m1 m2 m3)
                |> Maybe.withDefault (Error <| "Could not parse triangle indices from line: " ++ line)

        --    Maybe.map3
        --        Vmap3
        --        (parseVertMap (String.fromList v1))
        --        (parseVertMap m2)
        --        (parseVertMap m3)
        --        |> Maybe.withDefault (Error <| "Could not parse triangle indices from line: " ++ line)
        --Vmap3 (parseVertMap m1) (parseVertMap m2) (parseVertMap m3)
        --Vmap3 (parseVmaps line)
        'o' :: ' ' :: rest ->
            Ignored <| "o: " ++ String.fromList rest

        'g' :: ' ' :: rest ->
            Ignored <| "g: " ++ String.fromList rest

        's' :: ' ' :: rest ->
            Ignored <| "s: " ++ String.fromList rest

        'u' :: 's' :: 'e' :: 'm' :: 't' :: 'l' :: ' ' :: rest ->
            Ignored <| "usemtl: " ++ String.fromList rest

        'm' :: 't' :: 'l' :: 'l' :: 'i' :: 'b' :: ' ' :: rest ->
            Ignored <| "mtllib: " ++ String.fromList rest

        '#' :: ' ' :: rest ->
            Ignored <| "Comment..."

        [] ->
            Ignored <| "Empty line"

        x ->
            Error <| "Unknown entity: " ++ String.fromList x


parse : Options -> String -> List ( Vertex.Vertex, Vertex.Vertex, Vertex.Vertex )
parse options input =
    []
