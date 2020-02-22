module DDD.Parser.Obj exposing (parse)

import Array exposing (Array)
import DDD.Data.Vertex as Vertex exposing (Vertex)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Options =
    { scale : Float
    , color : Vec3
    }


type alias Data =
    { geometry : Array Vec3
    , texture : Array Vec2
    , normal : Array Vec3
    , vmap3 : List VertMap3
    , errors : List String
    }


type ObjData
    = Geometry Vec3
    | Texture Vec2
    | Normal Vec3
    | Vmap3 VertMap3
    | Error String


type alias VertMap3 =
    { m1 : VertMap, m2 : VertMap, m3 : VertMap }


type alias VertMap =
    { geometry : Int
    , texture : Maybe Int
    , normal : Maybe Int
    }


parse : Options -> String -> List ( Vertex, Vertex, Vertex )
parse options input =
    let
        init =
            Data
                Array.empty
                Array.empty
                Array.empty
                []
                []

        data : Data
        data =
            String.lines input
                |> List.foldl insertData init

        _ =
            [ ( "geometry ", Array.length data.geometry )
            , ( "texture  ", Array.length data.texture )
            , ( "normal   ", Array.length data.normal )
            , ( "vmap3    ", List.length data.vmap3 )
            , ( "errors   ", List.length data.errors )
            ]
                |> List.map (\( m, v ) -> Debug.log m v)

        _ =
            Debug.log "ERRORS" data.errors
    in
    verticesFromData options data


verticesFromData : Options -> Data -> List ( Vertex, Vertex, Vertex )
verticesFromData { scale, color } data =
    let
        verticesFromVmap3 : VertMap3 -> Maybe ( Vertex, Vertex, Vertex )
        verticesFromVmap3 vmap =
            case
                ( Array.get (vmap.m1.geometry - 1) data.geometry
                , Array.get (vmap.m2.geometry - 1) data.geometry
                , Array.get (vmap.m3.geometry - 1) data.geometry
                )
            of
                ( Just v1, Just v2, Just v3 ) ->
                    let
                        getNormal i =
                            Array.get (i - 1) data.normal

                        getUv i =
                            Array.get (i - 1) data.texture

                        calculateNormal v1_ v2_ v3_ =
                            Vec3.cross (Vec3.sub v1_ v2_) (Vec3.sub v1_ v3_) |> Vec3.normalize

                        ( vn1, vn2, vn3 ) =
                            ( vmap.m1.normal |> Maybe.andThen getNormal |> Maybe.withDefault (calculateNormal v1 v2 v3)
                            , vmap.m2.normal |> Maybe.andThen getNormal |> Maybe.withDefault (calculateNormal v2 v3 v1)
                            , vmap.m3.normal |> Maybe.andThen getNormal |> Maybe.withDefault (calculateNormal v3 v1 v2)
                            )

                        ( uv1, uv2, uv3 ) =
                            ( vmap.m1.texture |> Maybe.andThen getUv |> Maybe.withDefault (vec2 0 0)
                            , vmap.m2.texture |> Maybe.andThen getUv |> Maybe.withDefault (vec2 0 0)
                            , vmap.m3.texture |> Maybe.andThen getUv |> Maybe.withDefault (vec2 0 0)
                            )
                    in
                    ( (v1 |> Vec3.scale scale)
                        |> Vertex.vertex
                        |> Vertex.withNormal vn1
                        |> Vertex.withColor color
                        |> Vertex.withUV uv1
                    , (v2 |> Vec3.scale scale)
                        |> Vertex.vertex
                        |> Vertex.withNormal vn2
                        |> Vertex.withColor color
                        |> Vertex.withUV uv2
                    , (v3 |> Vec3.scale scale)
                        |> Vertex.vertex
                        |> Vertex.withNormal vn3
                        |> Vertex.withColor color
                        |> Vertex.withUV uv3
                    )
                        |> Just

                _ ->
                    Nothing

        vertices =
            data.vmap3
                |> List.map verticesFromVmap3
                |> List.filterMap identity
    in
    vertices


insertData : String -> Data -> Data
insertData x data =
    case parseLine x of
        Geometry vs ->
            { data | geometry = data.geometry |> Array.push vs }

        Texture vs ->
            { data | texture = data.texture |> Array.push vs }

        Normal vs ->
            { data | normal = data.normal |> Array.push vs }

        Vmap3 vmap ->
            { data | vmap3 = vmap :: data.vmap3 }

        Error str ->
            { data | errors = str :: data.errors }


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

        parseVmaps : String -> VertMap3
        parseVmaps str =
            let
                -- TOOD: Handle quads?
                vmaps =
                    str
                        |> String.dropLeft 2
                        |> String.trim
                        |> String.split " "
                        |> List.map
                            (\vm ->
                                let
                                    v =
                                        vm
                                            |> String.split "/"
                                            |> List.map String.toInt
                                in
                                case v of
                                    c1 :: c2 :: c3 :: _ ->
                                        VertMap (c1 |> Maybe.withDefault 0) c2 c3

                                    c1 :: c2 :: _ ->
                                        VertMap (c1 |> Maybe.withDefault 0) c2 Nothing

                                    c1 :: _ ->
                                        VertMap (c1 |> Maybe.withDefault 0) Nothing Nothing

                                    _ ->
                                        -- TOOD: Add to errors instead
                                        VertMap 0 Nothing Nothing
                            )
            in
            case vmaps of
                v1 :: v2 :: v3 :: _ ->
                    VertMap3 v1 v2 v3

                _ ->
                    -- TOOD: Add to errors instead
                    VertMap3
                        (VertMap 0 Nothing Nothing)
                        (VertMap 0 Nothing Nothing)
                        (VertMap 0 Nothing Nothing)
    in
    case String.left 2 line |> String.trim of
        "v" ->
            Geometry (parseVector line)

        "vt" ->
            Texture (parseVector2 line)

        "vn" ->
            Normal (parseVector line)

        "f" ->
            Vmap3 (parseVmaps line)

        x ->
            Error <| "Unknown entity: " ++ x
