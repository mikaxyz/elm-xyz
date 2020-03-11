module XYZMika.XYZ.Parser.Obj exposing (parse)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import XYZMika.Color as Color
import XYZMika.Debug as Dbug
import XYZMika.XYZ.Data.Vertex as Vertex


type alias Index =
    Int


type VertexIndices
    = Position Index
    | PositionUv Index Index
    | PositionNormal Index Index
    | PositionUvNormal Index Index Index


type Indexed a
    = Indexed a Index


type Vertex
    = V (Indexed Vec3)
    | VT (Indexed Vec3) (Indexed Vec2)
    | VN (Indexed Vec3) (Indexed Vec3)
    | VTN (Indexed Vec3) (Indexed Vec2) (Indexed Vec3)


toVertex : { scale : Float, color : Vec3 } -> Vertex -> Vertex.Vertex
toVertex { scale, color } x =
    case x of
        V (Indexed v vi) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withColor color

        VT (Indexed v vi) (Indexed uv uvi) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withUV uv
                |> Vertex.withColor color

        VN (Indexed v vi) (Indexed normal normali) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withNormal normal
                |> Vertex.withColor color

        VTN (Indexed v vi) (Indexed uv uvi) (Indexed normal normali) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withUV uv
                |> Vertex.withNormal normal
                |> Vertex.withColor color


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


type alias VertMap =
    { v : Int
    , vt : Maybe Int
    , vn : Maybe Int
    }


type alias Output =
    { triangles : List ( Vertex.Vertex, Vertex.Vertex, Vertex.Vertex )
    , indexedTriangles : ( List Vertex.Vertex, List ( Int, Int, Int ) )
    }


parse : Options -> String -> Output
parse options input =
    let
        indexed =
            String.lines input
                |> List.map parseLine
                |> indexedVertices
    in
    String.lines input
        |> List.map parseLine
        |> toVertices
        |> (\vertices ->
                { triangles =
                    vertices
                        |> List.map
                            (\( v1, v2, v3 ) ->
                                ( toVertex options v1
                                , toVertex options v2
                                , toVertex options v3
                                )
                            )
                , indexedTriangles = indexed
                }
           )


indexedVertices : List ObjData -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
indexedVertices data =
    let
        indices : ( VertMap, VertMap, VertMap ) -> ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
        indices ( v1, v2, v3 ) =
            ( ( v1.v, v1.vt |> Maybe.withDefault -1, v1.vn |> Maybe.withDefault -1 )
            , ( v2.v, v2.vt |> Maybe.withDefault -1, v2.vn |> Maybe.withDefault -1 )
            , ( v3.v, v3.vt |> Maybe.withDefault -1, v3.vn |> Maybe.withDefault -1 )
            )

        geometry =
            data
                |> List.filterMap mapGeometry
                |> Array.fromList

        uvs =
            data
                |> List.filterMap mapTexture
                |> Array.fromList

        normals =
            data
                |> List.filterMap mapNormal
                |> Array.fromList

        toVertex2 : ( Int, Int, Int ) -> Vertex.Vertex
        toVertex2 ( p1, t1, n1 ) =
            -- TODO: Handle maybes
            Maybe.map3
                (\p uv n -> Vertex.vertex p |> Vertex.withUV uv |> Vertex.withNormal n)
                (Array.get p1 geometry)
                (Array.get t1 uvs)
                (Array.get n1 normals)
                |> Maybe.withDefault (Vertex.vertex (vec3 0 0 0))

        collect :
            Dict ( Int, Int, Int ) Vertex.Vertex
            -> ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
            -> Dict ( Int, Int, Int ) Vertex.Vertex
        collect store ( v1, v2, v3 ) =
            store
                |> Dict.insert v1 (toVertex2 v1)
                |> Dict.insert v2 (toVertex2 v2)
                |> Dict.insert v3 (toVertex2 v3)

        triangles : List ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
        triangles =
            data
                |> List.filterMap mapVertMap
                |> List.map indices

        vertices : Dict ( Int, Int, Int ) Vertex.Vertex
        vertices =
            triangles
                |> List.map (Debug.log "indices")
                |> List.foldl (\x a -> collect a x) Dict.empty

        rewriteIndices :
            Int
            -> Dict ( Int, Int, Int ) Int
            -> Dict ( Int, Int, Int ) Vertex.Vertex
            -> ( Dict ( Int, Int, Int ) Vertex.Vertex, List ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) ) )
            -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
            -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
        rewriteIndices index indexStore vertexStore ( vs, is ) ( vsStore, isStore ) =
            case is of
                ( i1, i2, i3 ) :: rest ->
                    let
                        ( index1, indexStore1 ) =
                            indexStore
                                |> Dict.get i1
                                |> Maybe.map (\i -> ( index, indexStore ))
                                |> Maybe.withDefault ( index + 1, indexStore |> Dict.insert i1 index )

                        ( index2, indexStore2 ) =
                            indexStore1
                                |> Dict.get i2
                                |> Maybe.map (\i -> ( index1, indexStore1 ))
                                |> Maybe.withDefault ( index1 + 1, indexStore1 |> Dict.insert i2 index1 )

                        ( index3, indexStore3 ) =
                            indexStore2
                                |> Dict.get i3
                                |> Maybe.map (\i -> ( index2, indexStore2 ))
                                |> Maybe.withDefault ( index2 + 1, indexStore2 |> Dict.insert i3 index2 )

                        ( vs1, vsStore1 ) =
                            vs
                                |> Dict.get i1
                                |> Maybe.map
                                    (\v -> ( Dict.remove i1 vs, v :: vsStore ))
                                |> Maybe.withDefault ( vs, vsStore )

                        ( vs2, vsStore2 ) =
                            vs
                                |> Dict.get i2
                                |> Maybe.map
                                    (\v -> ( Dict.remove i2 vs1, v :: vsStore1 ))
                                |> Maybe.withDefault ( vs1, vsStore1 )

                        ( vs3, vsStore3 ) =
                            vs
                                |> Dict.get i3
                                |> Maybe.map
                                    (\v -> ( Dict.remove i3 vs2, v :: vsStore2 ))
                                |> Maybe.withDefault ( vs2, vsStore2 )

                        vertex1 : Vertex.Vertex
                        vertex1 =
                            vertexStore
                                |> Dict.get i1
                                |> Maybe.withDefault (i1 |> toVertex2)

                        vertex2 : Vertex.Vertex
                        vertex2 =
                            vertexStore
                                |> Dict.get i2
                                |> Maybe.withDefault (i2 |> toVertex2)

                        vertex3 : Vertex.Vertex
                        vertex3 =
                            vertexStore
                                |> Dict.get i3
                                |> Maybe.withDefault (i3 |> toVertex2)

                        vertexStore_ : Dict ( Int, Int, Int ) Vertex.Vertex
                        vertexStore_ =
                            vertexStore
                                |> Dict.insert i1 vertex1
                                |> Dict.insert i2 vertex2
                                |> Dict.insert i3 vertex3
                    in
                    rewriteIndices index3 indexStore3 vertexStore_ ( vs3, rest ) ( vsStore3, ( index1, index2, index3 ) :: isStore )

                [] ->
                    let
                        convertIndex : ( Int, Int, Int ) -> Int
                        convertIndex i =
                            indexStore
                                |> Dict.get i
                                |> Maybe.withDefault 0

                        convertedIndices : List ( Int, Int, Int )
                        convertedIndices =
                            data
                                |> List.filterMap mapVertMap
                                |> List.map indices
                                |> List.map (\( i1, i2, i3 ) -> ( convertIndex i1, convertIndex i2, convertIndex i3 ))

                        convertedVertices =
                            vertexStore
                                |> Dict.toList
                                |> List.map (\( i, v ) -> ( v, convertIndex i ))
                                |> List.sortBy Tuple.second
                                |> List.map Tuple.first
                    in
                    ( convertedVertices, convertedIndices )
    in
    rewriteIndices 0 Dict.empty Dict.empty ( vertices, triangles ) ( [], [] )
        |> (\( vs, is ) ->
                ( vs |> List.map (Debug.log "V")
                , is |> List.map (\( i1, i2, i3 ) -> Debug.log "I" ( i1 + 1, i2 + 1, i3 + 1 ) |> always ( i1, i2, i3 ))
                )
           )


toVertices : List ObjData -> List ( Vertex, Vertex, Vertex )
toVertices data =
    let
        geometry =
            data
                |> List.filterMap mapGeometry
                |> Array.fromList

        uvs =
            data
                |> List.filterMap mapTexture
                |> Array.fromList

        normals =
            data
                |> List.filterMap mapNormal
                |> Array.fromList

        toVertexData : VertMap -> Maybe Vertex
        toVertexData { v, vt, vn } =
            case ( v, vt, vn ) of
                ( iGeometry, Nothing, Nothing ) ->
                    Maybe.map (\p -> V (Indexed p iGeometry)) (Array.get iGeometry geometry)

                ( iGeometry, Just iUv, Nothing ) ->
                    Maybe.map2
                        (\p uv -> VT (Indexed p iGeometry) (Indexed uv iUv))
                        (Array.get iGeometry geometry)
                        (Array.get iUv uvs)

                ( iGeometry, Nothing, Just iNormal ) ->
                    Maybe.map2
                        (\p normal -> VN (Indexed p iGeometry) (Indexed normal iNormal))
                        (Array.get iGeometry geometry)
                        (Array.get iNormal normals)

                ( iGeometry, Just iUv, Just iNormal ) ->
                    Maybe.map3
                        (\p uv normal -> VTN (Indexed p iGeometry) (Indexed uv iUv) (Indexed normal iNormal))
                        (Array.get iGeometry geometry)
                        (Array.get iUv uvs)
                        (Array.get iNormal normals)

        toTriangles : ( VertMap, VertMap, VertMap ) -> Maybe ( Vertex, Vertex, Vertex )
        toTriangles ( m1, m2, m3 ) =
            Maybe.map3
                (\x y z -> ( x, y, z ))
                (toVertexData m1)
                (toVertexData m2)
                (toVertexData m3)
    in
    data
        --|> List.map (Dbug.log "data")
        |> List.filterMap mapVertMap
        --|> List.map (Dbug.log "VertMap")
        |> List.filterMap toTriangles



--|> List.indexedMap (\i x -> Dbug.log ((i |> String.fromInt |> String.padLeft 2 '0') ++ ": Triangle") x)
--|> List.length
--|> Dbug.log "Vertices count"
--|> always []


mapVertMap : ObjData -> Maybe ( VertMap, VertMap, VertMap )
mapVertMap x =
    case x of
        Vmap3 v1 v2 v3 ->
            Just ( v1, v2, v3 )

        _ ->
            Nothing


mapGeometry : ObjData -> Maybe Vec3
mapGeometry x =
    case x of
        Geometry v ->
            Just v

        _ ->
            Nothing


mapTexture : ObjData -> Maybe Vec2
mapTexture x =
    case x of
        Texture v ->
            Just v

        _ ->
            Nothing


mapNormal : ObjData -> Maybe Vec3
mapNormal x =
    case x of
        Normal v ->
            Just v

        _ ->
            Nothing



--|> List.map apply
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

        parseVertMap : String -> Maybe VertMap
        parseVertMap x =
            case String.split "/" x |> List.map String.toInt of
                v :: t :: n :: [] ->
                    v
                        |> Maybe.map (\v_ -> VertMap v_ t n |> zeroIndexVmap)

                v :: t :: [] ->
                    v
                        |> Maybe.map (\v_ -> VertMap v_ t Nothing |> zeroIndexVmap)

                xxx ->
                    let
                        _ =
                            Dbug.log "No idea" xxx
                    in
                    Nothing

        parseVertMaps : String -> Maybe ( VertMap, VertMap, VertMap )
        parseVertMaps x =
            case x |> String.trim |> String.split " " of
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

        'f' :: ' ' :: data ->
            parseVertMaps (String.fromList data)
                |> Maybe.map (\( m1, m2, m3 ) -> Vmap3 m1 m2 m3)
                |> Maybe.withDefault (Error <| "Could not parse triangle indices from line: " ++ line)

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
