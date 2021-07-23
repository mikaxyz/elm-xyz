module Tests.TangentSpace exposing (suite)

import Expect exposing (Expectation)
import Math.Vector2 as Vec2 exposing (vec2)
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 exposing (vec4)
import Test exposing (..)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Parser.Obj as Obj



-- http://tutorial.math.lamar.edu/Classes/CalcIII/3DSpace.aspx
--
-- https://learnopengl.com/Advanced-Lighting/Normal-Mapping
-- https://gamedev.stackexchange.com/questions/68612/how-to-compute-tangent-and-bitangent-vectors
-- https://www.marti.works/calculating-tangents-for-your-mesh/
-- http://blog.db-in.com/calculating-normals-and-tangent-space/
-- https://gamedev.stackexchange.com/questions/146855/how-do-you-compute-the-tangent-space-vectors-with-normals-given-in-the-mesh


suite : Test
suite =
    describe "Some vector math"
        [ test "generates tangents/bi tangents for normals" <|
            \_ ->
                let
                    ( v1, v2, v3 ) =
                        ( vec3 0 5 0
                        , vec3 5 5 0
                        , vec3 0 0 0
                        )

                    ( uv1, uv2, uv3 ) =
                        ( vec2 0 0
                        , vec2 1 0
                        , vec2 0 1
                        )

                    ( vN1, vN2, vN3 ) =
                        ( vec3 0 0 1
                        , vec3 0 0 1
                        , vec3 0 0 1
                        )

                    ( dv21, dv31 ) =
                        ( Vec3.sub v2 v1
                        , Vec3.sub v3 v1
                        )

                    ( duv21, duv31 ) =
                        ( Vec2.sub uv2 uv1
                        , Vec2.sub uv3 uv1
                        )

                    r =
                        let
                            ( duv1, duv2 ) =
                                ( Vec2.toRecord duv21
                                , Vec2.toRecord duv31
                                )
                        in
                        1.0 / ((duv1.x * duv2.y) - (duv1.y * duv2.x))

                    vT1 =
                        let
                            ( duv1, duv2 ) =
                                ( Vec2.toRecord duv21
                                , Vec2.toRecord duv31
                                )
                        in
                        Vec3.sub (Vec3.scale duv2.y dv21) (Vec3.scale duv1.y dv31)
                            |> Vec3.scale r
                            |> Vec3.normalize

                    vT2 =
                        let
                            ( duv1, duv2 ) =
                                ( Vec2.toRecord duv21
                                , Vec2.toRecord duv31
                                )
                        in
                        Vec3.sub (Vec3.scale duv2.y dv21) (Vec3.scale duv1.y dv31)
                            |> Vec3.scale r
                            |> Vec3.normalize

                    expected =
                        { dv21 = vec3 5 0 0
                        , dv31 = vec3 0 -5 0
                        , duv21 = vec2 1 0
                        , duv31 = vec2 0 1
                        , vT1 = vec3 1 0 0
                        , r = 1
                        }
                in
                Expect.equal
                    expected
                    { dv21 = dv21
                    , dv31 = dv31
                    , duv21 = duv21
                    , duv31 = duv31
                    , vT1 = vT1
                    , r = r
                    }
        , test "generates normals/tangents/bi tangents for triangle" <|
            \_ ->
                let
                    -- Calculate with no existing normals...
                    -- https://www.gamasutra.com/view/feature/129939/messing_with_tangent_space.php?print=1
                    ( v1, v2, v3 ) =
                        ( vec3 0 20 0
                        , vec3 20 20 0
                        , vec3 0 0 0
                        )

                    ( uv1, uv2, uv3 ) =
                        ( vec2 0 0
                        , vec2 1 0
                        , vec2 0 1
                        )

                    ( dv21, dv31 ) =
                        ( Vec3.sub v2 v1
                        , Vec3.sub v3 v1
                        )

                    ( duv21, duv31 ) =
                        ( Vec2.sub uv2 uv1
                        , Vec2.sub uv3 uv1
                        )

                    vT =
                        let
                            ( { x, y, z }, u ) =
                                ( Vec3.toRecord dv21
                                , Vec2.getX duv21
                                )
                        in
                        vec3 (x / u) (y / u) (z / u)
                            |> Vec3.normalize

                    vN =
                        Vec3.cross dv21 dv31
                            |> Vec3.normalize

                    vB =
                        Vec3.cross vT vN
                            |> Vec3.normalize

                    expected =
                        { dv21 = vec3 20 0 0
                        , dv31 = vec3 0 -20 0
                        , duv21 = vec2 1 0
                        , duv31 = vec2 0 1
                        , vT = vec3 1 0 0
                        , vN = vec3 0 0 -1
                        , vB = vec3 0 1 0
                        }
                in
                Expect.equal
                    expected
                    { dv21 = dv21
                    , dv31 = dv31
                    , duv21 = duv21
                    , duv31 = duv31
                    , vT = vT
                    , vN = vN
                    , vB = vB
                    }
        ]
