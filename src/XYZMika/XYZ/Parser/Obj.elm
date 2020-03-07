module XYZMika.XYZ.Parser.Obj exposing (parse)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import XYZMika.Color as Color
import XYZMika.XYZ.Data.Vertex as Vertex


log : String -> a -> a
log m a =
    if 1 == 1 then
        Debug.log m a

    else
        a


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

        VN (Indexed v vi) (Indexed normal normali) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withNormal normal

        VTN (Indexed v vi) (Indexed uv uvi) (Indexed normal normali) ->
            Vertex.vertex (v |> Vec3.scale scale)
                |> Vertex.withUV uv
                |> Vertex.withNormal normal


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
    String.lines input
        |> List.map parseLine
        |> toVertices
        |> (\vertices ->
                { triangles = vertices |> List.map (\( v1, v2, v3 ) -> ( toVertex options v1, toVertex options v2, toVertex options v3 ))
                , indexedTriangles = toVerticesWithIndex vertices
                }
           )



--parseIndexed : Options -> String -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
--parseIndexed options input =
--    String.lines input
--        |> List.map parseLine
--        --|> List.map (log "line")
--        |> toVertices
--        --|> List.map (log "data")
--        |> always ( [], [] )


toVerticesWithIndex : List ( Vertex, Vertex, Vertex ) -> ( List Vertex.Vertex, List ( Int, Int, Int ) )
toVerticesWithIndex vertices =
    vertices
        --|> List.foldl (\x (vertices, indicies) ->
        --let
        --    case x of
        --        (v1, v2, v3) ->
        --        if List.member v1 vertices then
        --
        --in
        --    acc
        --
        --) ( [], [] )
        |> always ( [], [] )


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
        |> List.filterMap mapVertMap
        --|> List.map (log "VertMap")
        |> List.filterMap toTriangles
        |> List.indexedMap (\i x -> log ((i |> String.fromInt |> String.padLeft 2 '0') ++ ": Vertex") x)



--|> List.length
--|> log "Vertices count"
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

                _ ->
                    Nothing

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
