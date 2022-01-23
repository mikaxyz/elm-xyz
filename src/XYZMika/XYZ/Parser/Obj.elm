module XYZMika.XYZ.Parser.Obj exposing (parse)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.Debug as Dbug
import XYZMika.XYZ.Data.Vertex as Vertex


type alias Options =
    { scale : Float
    , color : Vec3
    , transform : Mat4
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
        indexed : ( List Vertex.Vertex, List ( Int, Int, Int ) )
        indexed =
            String.lines input
                |> List.map (parseLine options.transform)
                |> indexedVertices options

        vertices_ : Array Vertex.Vertex
        vertices_ =
            Tuple.first indexed
                |> Array.fromList

        triangles : List ( Vertex.Vertex, Vertex.Vertex, Vertex.Vertex )
        triangles =
            Tuple.second indexed
                |> List.filterMap
                    (\( i1, i2, i3 ) ->
                        Maybe.map3
                            (\v1 v2 v3 -> ( v1, v2, v3 ))
                            (Array.get i1 vertices_)
                            (Array.get i2 vertices_)
                            (Array.get i3 vertices_)
                    )
    in
    { triangles = triangles
    , indexedTriangles = generateTangents indexed
    }


generateTangents : ( List Vertex.Vertex, List ( Int, Int, Int ) ) -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
generateTangents ( vs, is ) =
    ( vs
        |> List.indexedMap (generateTangent is (vs |> Array.fromList))
    , is
    )


generateTangent : List ( Int, Int, Int ) -> Array Vertex.Vertex -> Int -> Vertex.Vertex -> Vertex.Vertex
generateTangent indices vertices index vertex =
    let
        triangles : List ( Vertex.Vertex, Vertex.Vertex, Vertex.Vertex )
        triangles =
            indices
                |> List.filter (\( i1, i2, i3 ) -> [ i1, i2, i3 ] |> List.member index)
                |> List.map
                    (\( i1, i2, i3 ) ->
                        if index == i1 then
                            ( i1, i2, i3 )

                        else if index == i2 then
                            ( i2, i1, i3 )

                        else
                            ( i3, i1, i2 )
                    )
                |> List.filterMap
                    (\( i1, i2, i3 ) ->
                        Maybe.map3
                            (\v1 v2 v3 -> ( v1, v2, v3 ))
                            (Array.get i1 vertices)
                            (Array.get i2 vertices)
                            (Array.get i3 vertices)
                    )

        --|> Debug.log "triangles"
        tangent : ( Vertex.Vertex, Vertex.Vertex, Vertex.Vertex ) -> Vec3
        tangent ( v1, v2, v3 ) =
            let
                -- TODO: Calculate tangents for triangle
                --{ uv1, uv2, uv2 } =
                --{ uv1x, uv1y, uv2x, uv2y, uv3x, uv3y } =
                --    { uv1x = v1.uv |> Vec2.getX
                --    , uv1y = v1.uv |> Vec2.getY
                --    , uv2x = v2.uv |> Vec2.getX
                --    , uv2y = v2.uv |> Vec2.getY
                --    , uv3x = v3.uv |> Vec2.getX
                --    , uv3y = v3.uv |> Vec2.getY
                --    }
                deltaPos1 : Vec3
                deltaPos1 =
                    Vec3.sub v1.position v2.position

                --|> Vec3.toRecord
                deltaPos2 : Vec3
                deltaPos2 =
                    Vec3.sub v1.position v3.position

                --|> Vec3.toRecord
                deltaUV1 : { x : Float, y : Float }
                deltaUV1 =
                    Vec2.sub v1.uv v2.uv
                        |> Vec2.toRecord

                --|> Debug.log "deltaUV1"
                deltaUV2 : { x : Float, y : Float }
                deltaUV2 =
                    Vec2.sub v1.uv v3.uv
                        --|> Debug.log "deltaUV2"
                        |> Vec2.toRecord

                r : Float
                r =
                    1.0
                        / (deltaUV1.x * deltaUV2.y - deltaUV1.y * deltaUV2.x)

                --|> Debug.log "r"
                d1 =
                    Vec3.scale deltaUV2.y deltaPos1

                d2 =
                    Vec3.scale deltaUV1.y deltaPos2

                tangent1 =
                    Vec3.sub d1 d2 |> Vec3.scale r

                --Vec3.sub (Vec3.scale deltaUV1.y deltaPos2) (Vec3.scale deltaUV2.y deltaPos1)
                --|> Vec3.scale r
                --|> Debug.log "tangent"
                --bitangent1 =
                --    Vec3.sub (Vec3.scale deltaUV1.x deltaPos2) (Vec3.scale deltaUV2.x deltaPos1)
                --|> Vec3.scale r
                --|> Debug.log "bitangent1"
                --*r;
                --bitangent = (deltaPos2 * deltaUV1.x   - deltaPos1 * deltaUV2.x)*r;
            in
            Vec3.normalize tangent1
    in
    --|> Vertex.withTangent tangent_
    triangles
        |> List.head
        -- TODO: Dont just pick head, Create an average tangent from all triangles
        |> Maybe.map
            (\t ->
                vertex
                    --|> Vertex.withNormal (normal t)
                    |> Vertex.withTangent (tangent t)
             --|> Vertex.withTangent tangent_
            )
        |> Maybe.withDefault vertex


indexedVertices : Options -> List ObjData -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
indexedVertices options data =
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

        tangentVector : Vec3 -> ( Float, Float ) -> Vec3
        tangentVector p ( u, v ) =
            vec3 0 0 0

        toVertex2 : ( Int, Int, Int ) -> Vertex.Vertex
        toVertex2 ( vGeometry, vUv, vNormal ) =
            -- TODO: Handle maybes (t + n are now parsed as -1) properly
            case ( Array.get vGeometry geometry, Array.get vUv uvs, Array.get vNormal normals ) of
                ( Just p, Nothing, Nothing ) ->
                    Vertex.vertex (p |> Vec3.scale options.scale)
                        |> Vertex.withColor options.color

                ( Just p, Just uv, Nothing ) ->
                    Vertex.vertex (p |> Vec3.scale options.scale)
                        |> Vertex.withColor options.color
                        |> Vertex.withUV uv

                ( Just p, Nothing, Just normal ) ->
                    Vertex.vertex (p |> Vec3.scale options.scale)
                        |> Vertex.withColor options.color
                        |> Vertex.withNormal normal

                ( Just p, Just uv, Just normal ) ->
                    Vertex.vertex (p |> Vec3.scale options.scale)
                        |> Vertex.withColor options.color
                        |> Vertex.withUV uv
                        |> Vertex.withNormal normal

                _ ->
                    -- TODO: This should not be an option
                    Vertex.vertex (vec3 0 0 0)

        triangles : List ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
        triangles =
            data
                |> List.filterMap mapVertMap
                |> List.map indices

        rewriteIndices :
            Int
            -> Dict ( Int, Int, Int ) Int
            -> Dict ( Int, Int, Int ) Vertex.Vertex
            -> List ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
            -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
        rewriteIndices index indexStore vertexStore is =
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

                        calculateNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
                        calculateNormal v1_ v2_ v3_ =
                            Vec3.cross (Vec3.sub v1_ v2_) (Vec3.sub v1_ v3_) |> Vec3.normalize

                        calculateMissingNormal : Vec3 -> Vec3 -> Vec3 -> Vertex.Vertex -> Vertex.Vertex
                        calculateMissingNormal v1 v2 v3 vertex =
                            if vertex.meta.hasNormal then
                                vertex

                            else
                                vertex |> Vertex.withNormal (calculateNormal v1 v2 v3)

                        vertexStore_ : Dict ( Int, Int, Int ) Vertex.Vertex
                        vertexStore_ =
                            vertexStore
                                |> Dict.insert i1
                                    (vertex1
                                        |> calculateMissingNormal
                                            vertex1.position
                                            vertex2.position
                                            vertex3.position
                                    )
                                |> Dict.insert i2
                                    (vertex2
                                        |> calculateMissingNormal
                                            vertex1.position
                                            vertex2.position
                                            vertex3.position
                                    )
                                |> Dict.insert i3
                                    (vertex3
                                        |> calculateMissingNormal
                                            vertex1.position
                                            vertex2.position
                                            vertex3.position
                                    )
                    in
                    rewriteIndices index3 indexStore3 vertexStore_ rest

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
    rewriteIndices 0 Dict.empty Dict.empty triangles


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


parseLine : Mat4 -> String -> ObjData
parseLine transform line =
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
                |> Mat4.transform transform

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

                v :: [] ->
                    v
                        |> Maybe.map (\v_ -> VertMap v_ Nothing Nothing |> zeroIndexVmap)

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
