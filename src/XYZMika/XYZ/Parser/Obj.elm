module XYZMika.XYZ.Parser.Obj exposing (parse, parseIndexed)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.Color as Color
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)


type alias Options =
    { scale : Float
    , color : Vec3
    }


type alias IndexedData =
    { vertices : Array Vec3
    , normals : Array Vec3
    , uvs : Array Vec2

    -- TODO: Remove. Only keep indices...
    , vertexMap : List ( Int, Int, Int )
    , normalMap : List ( Int, Int, Int )
    , uvMap : List ( Int, Int, Int )
    , indices : List VertMap
    , textureIndexMap : Dict Int Int
    , normalIndexMap : Dict Int Int
    , vmap3 : List VertMap3

    --, tmap : Array (Int, Int, Int)
    }



--type alias VertCollected =
--    { geometry : Vec3
--    , texture : Vec3
--    , normal : Vec3
--    }


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


parseIndexed : Options -> String -> ( List Vertex, List ( Int, Int, Int ) )
parseIndexed options input =
    let
        init =
            IndexedData
                Array.empty
                Array.empty
                Array.empty
                []
                []
                []
                []
                Dict.empty
                Dict.empty
                []

        data : IndexedData
        data =
            String.lines input
                |> List.foldl insertIndexedData init

        --|> Debug.log "data"
        _ =
            [ ( "vertices ", Array.length data.vertices )
            , ( "vertexMap   ", List.length data.vertexMap )
            , ( "normals  ", Array.length data.normals )
            , ( "normalMap    ", List.length data.normalMap )
            , ( "uvs  ", Array.length data.uvs )
            , ( "uvMap    ", List.length data.uvMap )

            --
            , ( "indices    ", List.length data.indices )

            --, ( "errors   ", List.length data.errors )
            ]
                |> List.map (\( m, v ) -> Debug.log m v)

        --_ =
        --    Debug.log "ERRORS" data.errors
    in
    verticesIndexedFromData options data


insertIndexedData : String -> IndexedData -> IndexedData
insertIndexedData line data =
    case parseLine line of
        Geometry vs ->
            { data | vertices = data.vertices |> Array.push vs }

        Texture vs ->
            { data | uvs = data.uvs |> Array.push vs }

        Normal vs ->
            { data | normals = data.normals |> Array.push vs }

        Vmap3 { m1, m2, m3 } ->
            { data
                | vertexMap = (( m1.geometry, m2.geometry, m3.geometry ) |> Debug.log "vmap") :: data.vertexMap
                , vmap3 = VertMap3 m1 m2 m3 :: data.vmap3
                , normalIndexMap =
                    Maybe.map3
                        (\n1 n2 n3 ->
                            data.normalIndexMap
                                |> Dict.insert m1.geometry n1
                                |> Dict.insert m2.geometry n2
                                |> Dict.insert m2.geometry n3
                        )
                        m1.normal
                        m2.normal
                        m3.normal
                        |> Maybe.withDefault data.normalIndexMap
                , indices =
                    VertMap m1.geometry m1.texture m1.normal
                        :: VertMap m2.geometry m2.texture m2.normal
                        :: VertMap m3.geometry m3.texture m3.normal
                        :: data.indices
            }
                |> (\data_ ->
                        Maybe.map3
                            (\i1 i2 i3 ->
                                { data_
                                    | normalMap = ( i1, i2, i3 ) :: data_.normalMap
                                }
                            )
                            m1.normal
                            m2.normal
                            m3.normal
                   )
                |> Maybe.withDefault data
                |> (\data_ ->
                        Maybe.map3
                            (\i1 i2 i3 ->
                                { data_
                                    | uvMap = ( i1, i2, i3 ) :: data_.uvMap
                                }
                            )
                            m1.texture
                            m2.texture
                            m3.texture
                   )
                |> Maybe.withDefault data

        Error str ->
            let
                _ =
                    Debug.log "ERROR" str
            in
            data



--{ data | errors = str :: data.errors }


verticesIndexedFromData : Options -> IndexedData -> ( List Vertex, List ( Int, Int, Int ) )
verticesIndexedFromData { scale, color } data =
    let
        toVertex : Maybe Int -> Maybe Int -> Vec3 -> Vertex
        toVertex normal uv v =
            v
                |> Vec3.scale scale
                |> Vertex.vertex
                |> Vertex.withColor color
                |> (\vertex ->
                        normal
                            |> Maybe.andThen (\x -> Array.get x data.normals)
                            |> Maybe.map (\normal_ -> Vertex.withNormal normal_ vertex)
                            |> Maybe.withDefault vertex
                   )
                |> (\vertex ->
                        uv
                            |> Maybe.andThen (\x -> Array.get x data.uvs)
                            |> Maybe.map (\uv_ -> Vertex.withUV uv_ vertex)
                            |> Maybe.withDefault vertex
                   )

        --|> (\vertex ->
        --        normal
        --            |> Maybe.map (\normal_ ->
        --                (Array.get (normal_ - 1) data.normals)
        --                |> Maybe.map ()
        --                vertex |> Vertex.withNormal normal_)
        --            |> Maybe.withDefault vertex
        --   )
        vertexFromIndices : VertMap -> Maybe ( Int, Vertex )
        vertexFromIndices { geometry, texture, normal } =
            Array.get geometry data.vertices
                |> Maybe.map (\v -> ( geometry, toVertex normal texture v ))

        --|> Debug.log "VERTEX"
        --case
        --    ( Array.get (geometry - 1) data.vertices
        --    , Array.get (geometry - 1) data.vertices
        --    , Array.get (geometry - 1) data.vertices
        --    )
        --of
        --    ( Just p1, Just p2, Just p3 ) ->
        --        [ p1, p2, p3 ] |> List.map toVertex
        --position
        --    |> Vec3.scale scale
        --    |> Vertex.vertex
        --    |> Vertex.withColor color
        --    |> (\vertex ->
        --            data.normals
        --                |> Array.get (normal - 1)
        --                |> Maybe.map (\x -> vertex |> Vertex.withNormal x)
        --                |> Maybe.withDefault vertex
        --       )
        --    |> (\vertex ->
        --            data.uvs
        --                |> Array.get (uv - 1)
        --                |> Maybe.map (\x -> vertex |> Vertex.withUV x)
        --                |> Maybe.withDefault vertex
        --       )
        --    |> Debug.log ("VERT: " ++ String.fromInt v)
        --    |> Just
        --_ ->
        --    []
        --_ =
        --    data.vertices |> Array.map (\asd -> Debug.log "VERTS" asd)
        vertices : List ( Int, Vertex )
        vertices =
            data.indices
                |> List.filterMap vertexFromIndices

        --|> Array.fromList
        --averageTriangle : List Vertex -> Vertex
        averageVertex : List Vertex -> Vertex
        averageVertex vs =
            vs
                |> List.foldl
                    (\x acc ->
                        { acc
                            | position = x.position
                            , uv = x.uv
                            , normal = acc.normal |> Vec3.add x.normal |> Vec3.normalize

                            --, normal = x.normal
                        }
                    )
                    (Vertex.vertex Vec3.i)

        xxx : List ( Int, Vertex )
        xxx =
            data.vertexMap
                |> List.map
                    (\( v1, v2, v3 ) ->
                        let
                            vs1 =
                                vertices
                                    |> List.filter (\( i, _ ) -> i == v1)
                                    |> List.map Tuple.second
                                    |> averageVertex
                                    |> Tuple.pair v1

                            vs2 =
                                vertices
                                    |> List.filter (\( i, _ ) -> i == v2)
                                    |> List.map Tuple.second
                                    |> averageVertex
                                    |> Tuple.pair v2

                            vs3 =
                                vertices
                                    |> List.filter (\( i, _ ) -> i == v3)
                                    |> List.map Tuple.second
                                    |> averageVertex
                                    |> Tuple.pair v3

                            _ =
                                Debug.log "( v1, v2, v3 )" ( v1, v2, v3 )

                            --_ =
                            --    Debug.log "vs" (vs |> List.length)
                        in
                        [ vs1, vs2, vs3 ]
                    )
                |> List.concat

        unique =
            xxx
                |> Dict.fromList

        --_ =
        --    data.vertexMap
        --        |> List.length
        --        |> Debug.log "data.vertexMap"
        --
        --_ =
        --    unique |> Dict.values |> List.length |> Debug.log "UNIQUE"
        toTriangle : ( Int, Int, Int ) -> Maybe ( Vertex, Vertex, Vertex )
        toTriangle ( v1, v2, v3 ) =
            case
                ( data.vertices |> Array.get v1
                , data.vertices |> Array.get v2
                , data.vertices |> Array.get v3
                )
            of
                ( Just p1, Just p2, Just p3 ) ->
                    Just ( Vertex.vertex p1, Vertex.vertex p2, Vertex.vertex p3 )

                _ ->
                    Nothing

        verts : Dict Int Vertex
        verts =
            Dict.empty

        toVertexWithUvAndNormal : Vec3 -> Vec2 -> Vec3 -> Vertex
        toVertexWithUvAndNormal v uv normal =
            Vertex.vertex v
                |> Vertex.withColor (Color.cyan |> Color.toVec3)
                |> Vertex.withUV uv
                |> Vertex.withNormal normal

        --|> (\vertex ->
        --        uv
        --            --|> Maybe.andThen (\x -> Array.get x data.uvs)
        --            |> Maybe.map (\uv_ -> Vertex.withUV uv_ vertex)
        --            |> Maybe.withDefault vertex
        --   )
        storeVertex i store =
            Maybe.map3 (\p uv normal -> Dict.insert i (toVertexWithUvAndNormal p uv normal) store)
                (Array.get i data.vertices)
                (Array.get i data.uvs |> Debug.log ("uv    : " ++ String.fromInt i))
                (Array.get i data.normals |> Debug.log ("normal: " ++ String.fromInt i))
                |> Maybe.withDefault store

        collectVertices : Dict Int Vertex -> List ( Int, Int, Int ) -> Dict Int Vertex
        collectVertices store source =
            case source of
                ( i1, i2, i3 ) :: rest ->
                    store
                        |> storeVertex i1
                        |> storeVertex i2
                        |> storeVertex i3
                        |> (\s -> collectVertices s rest)

                [] ->
                    store

        collected =
            collectVertices Dict.empty data.vertexMap |> Dict.values

        _ =
            Debug.log "collected" (List.length collected)
    in
    --( unique |> Dict.values
    --( data.indices
    --    |> List.filterMap vertexFromIndices
    --    |> List.map Tuple.second
    ( collected
    , data.vertexMap
      --|> List.map (\x -> Debug.log "vmap" x)
      --|> List.map (\( v1, v2, v3 ) -> Debug.log "vmap" ( v1 - 1, v2 - 1, v3 - 1 ))
    )



--( data.vertices |> Array.toList |> List.map toVertex, data.vertexMap )


verticesFromData : Options -> Data -> List ( Vertex, Vertex, Vertex )
verticesFromData { scale, color } data =
    let
        verticesFromVmap3 : VertMap3 -> Maybe ( Vertex, Vertex, Vertex )
        verticesFromVmap3 vmap =
            case
                ( Array.get vmap.m1.geometry data.geometry
                , Array.get vmap.m2.geometry data.geometry
                , Array.get vmap.m3.geometry data.geometry
                )
            of
                ( Just v1, Just v2, Just v3 ) ->
                    let
                        getNormal i =
                            Array.get i data.normal

                        getUv i =
                            Array.get i data.texture

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

        zeroIndexVmap : VertMap -> VertMap
        zeroIndexVmap { geometry, texture, normal } =
            VertMap (geometry - 1)
                (texture |> Maybe.map (\x -> x - 1))
                (normal |> Maybe.map (\x -> x - 1))

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
                        |> List.map zeroIndexVmap
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
