module XYZMika.XYZ.Parser.Serialize exposing (decode, encode, toJsonString)

import Json.Decode as JD
import Json.Encode as JE
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)


type alias Output =
    { triangles : List ( Vertex, Vertex, Vertex )
    , indexedTriangles : ( List Vertex, List ( Int, Int, Int ) )
    }


toJsonString : Output -> String
toJsonString parsed =
    encode parsed |> JE.encode 0


encode : Output -> JE.Value
encode parsed =
    JE.object
        [ ( "triangles", encodeTriangles parsed.triangles )
        , ( "indexedTriangles", encodeIndexedTriangles parsed.indexedTriangles )
        ]


decode : String -> Result JD.Error Output
decode =
    JD.decodeString decoder



-- DECODERS


decoder : JD.Decoder Output
decoder =
    JD.map2 Output
        (JD.field "triangles" (JD.list triangleDecoder))
        (JD.field "indexedTriangles" indexedTrianglesDecoder)


triangleDecoder : JD.Decoder ( Vertex, Vertex, Vertex )
triangleDecoder =
    JD.map3 (\v1 v2 v3 -> ( v1, v2, v3 ))
        (JD.field "v1" vertexDecoder)
        (JD.field "v2" vertexDecoder)
        (JD.field "v3" vertexDecoder)


indexedTrianglesDecoder : JD.Decoder ( List Vertex, List ( Int, Int, Int ) )
indexedTrianglesDecoder =
    let
        triangleIndicesDecoder : JD.Decoder ( Int, Int, Int )
        triangleIndicesDecoder =
            JD.map3 (\i1 i2 i3 -> ( i1, i2, i3 ))
                (JD.field "i1" JD.int)
                (JD.field "i2" JD.int)
                (JD.field "i3" JD.int)
    in
    JD.map2 Tuple.pair
        (JD.field "vertices" (JD.list vertexDecoder))
        (JD.field "indices" (JD.list triangleIndicesDecoder))


vertexDecoder : JD.Decoder Vertex
vertexDecoder =
    let
        metaDecoder : JD.Decoder { hasColor : Bool, hasNormal : Bool, hasTangent : Bool, hasUV : Bool, hasUV1 : Bool }
        metaDecoder =
            JD.map5 (\a b c d e -> { hasColor = a, hasNormal = b, hasTangent = c, hasUV = d, hasUV1 = e })
                (JD.at [ "meta", "hasColor" ] JD.bool)
                (JD.at [ "meta", "hasNormal" ] JD.bool)
                (JD.at [ "meta", "hasColor" ] JD.bool)
                (JD.at [ "meta", "hasUV" ] JD.bool)
                (JD.at [ "meta", "hasUV1" ] JD.bool)
    in
    metaDecoder
        |> JD.andThen
            (\meta ->
                JD.map8
                    (\position color normal tangent uv0 uv1 weights joints ->
                        { position = position
                        , color = color
                        , normal = normal
                        , tangent = tangent
                        , uv = uv0
                        , uv1 = uv1
                        , meta = meta
                        , weights = weights
                        , joints = joints
                        }
                    )
                    (JD.field "position" vec3Decoder)
                    (JD.field "color" vec3Decoder)
                    (JD.field "normal" vec3Decoder)
                    (JD.field "tangent" vec3Decoder)
                    (JD.field "uv" vec2Decoder)
                    (JD.field "uv1" vec2Decoder)
                    (JD.maybe (JD.field "weights" vec4Decoder) |> JD.map (Maybe.withDefault (vec4 1 1 1 1)))
                    (JD.maybe (JD.field "joints" vec4Decoder) |> JD.map (Maybe.withDefault (vec4 1 1 1 1)))
            )


vec2Decoder : JD.Decoder Vec2
vec2Decoder =
    JD.map2 vec2
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)


vec3Decoder : JD.Decoder Vec3
vec3Decoder =
    JD.map3 vec3
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "z" JD.float)


vec4Decoder : JD.Decoder Vec4
vec4Decoder =
    JD.map4 vec4
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "z" JD.float)
        (JD.field "w" JD.float)



-- ENCODE


encodeTriangles : List ( Vertex, Vertex, Vertex ) -> JE.Value
encodeTriangles triangles =
    JE.list
        (\( v1, v2, v3 ) ->
            JE.object
                [ ( "v1", encodeVertex v1 )
                , ( "v2", encodeVertex v2 )
                , ( "v3", encodeVertex v3 )
                ]
        )
        triangles


encodeIndexedTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> JE.Value
encodeIndexedTriangles ( vertices, indices ) =
    JE.object
        [ ( "vertices", JE.list encodeVertex vertices )
        , ( "indices"
          , JE.list
                (\( i1, i2, i3 ) ->
                    JE.object
                        [ ( "i1", JE.int i1 )
                        , ( "i2", JE.int i2 )
                        , ( "i3", JE.int i3 )
                        ]
                )
                indices
          )
        ]



--


encodeVertex : Vertex -> JE.Value
encodeVertex vertex =
    JE.object
        [ ( "position", encodeVec3 vertex.position )
        , ( "color", encodeVec3 vertex.color )
        , ( "normal", encodeVec3 vertex.normal )
        , ( "tangent", encodeVec3 vertex.tangent )
        , ( "uv", encodeVec2 vertex.uv )
        , ( "weights", encodeVec4 vertex.weights )
        , ( "joints", encodeVec4 vertex.joints )
        , ( "meta"
          , JE.object
                [ ( "hasColor", JE.bool vertex.meta.hasColor )
                , ( "hasNormal", JE.bool vertex.meta.hasNormal )
                , ( "hasTangent", JE.bool vertex.meta.hasTangent )
                , ( "hasUV", JE.bool vertex.meta.hasUV )
                , ( "hasUV1", JE.bool vertex.meta.hasUV1 )
                ]
          )
        ]


encodeVec4 : Vec4 -> JE.Value
encodeVec4 vec =
    let
        { x, y, z, w } =
            Vec4.toRecord vec
    in
    JE.object
        [ ( "x", JE.float x )
        , ( "y", JE.float y )
        , ( "z", JE.float z )
        , ( "w", JE.float w )
        ]


encodeVec3 : Vec3 -> JE.Value
encodeVec3 vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
    JE.object
        [ ( "x", JE.float x )
        , ( "y", JE.float y )
        , ( "z", JE.float z )
        ]


encodeVec2 : Vec2 -> JE.Value
encodeVec2 vec =
    let
        { x, y } =
            Vec2.toRecord vec
    in
    JE.object
        [ ( "x", JE.float x )
        , ( "y", JE.float y )
        ]
