module DDD.Mesh.Landscape exposing (Options, simple)

import DDD.Data.Vertex as Vertex exposing (Vertex)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Options =
    { divisions : Int
    , width : Float
    , length : Float
    , height : Float
    , color : Float -> Vec3
    , elevation : Float -> Float -> Float
    }


simple : Options -> ( List Vertex, List ( Int, Int, Int ) )
simple options =
    let
        f =
            options.elevation

        c1 =
            { x = -options.width, y = options.length }

        c2 =
            { x = options.width, y = -options.length }

        vMap =
            List.range 0 ((options.divisions - 1) * (options.divisions - 1) - 1)
                |> List.map (quad (options.divisions - 1))
                |> List.concat

        vertices =
            points options.divisions (vec2 c1.x c1.y) (vec2 c2.x c2.y)
                |> List.map (addElevation options f)
                |> List.map
                    (\( elevation, pos, normal ) ->
                        Vertex.vertex pos
                            |> Vertex.withColor (options.color elevation)
                            |> Vertex.withNormal normal
                    )
    in
    ( vertices, vMap )


points : Int -> Vec2 -> Vec2 -> List Vec2
points div v1 v2 =
    List.range 0 (div * div - 1)
        |> List.map
            (\i ->
                let
                    ( ix, iy ) =
                        ( toFloat (modBy div i), toFloat (i // div) )

                    x =
                        Vec2.getX v1 + (ix / toFloat (div - 1) * (Vec2.getX v2 - Vec2.getX v1))

                    y =
                        Vec2.getY v1 + (iy / toFloat (div - 1) * (Vec2.getY v2 - Vec2.getY v1))
                in
                vec2 x y
            )


quad : Int -> Int -> List ( Int, Int, Int )
quad div i =
    let
        rr =
            (i // div)
                * (div + 1)

        ii =
            modBy div i + rr
    in
    [ ( ii, ii + 1, ii + div + 1 )
    , ( ii + 1, ii + div + 1, ii + div + 2 )
    ]


addElevation : Options -> (Float -> Float -> Float) -> Vec2 -> ( Float, Vec3, Vec3 )
addElevation options f v =
    let
        ( stepX, stepY ) =
            ( options.width / toFloat options.divisions
            , options.length / toFloat options.divisions
            )

        z =
            f (Vec2.getX v) (Vec2.getY v)

        elevationNorth =
            f (Vec2.getX v) (Vec2.getY v + stepY)

        elevationSouth =
            f (Vec2.getX v) (Vec2.getY v - stepY)

        elevationWest =
            f (Vec2.getX v + stepX) (Vec2.getY v)

        elevationEast =
            f (Vec2.getX v - stepX) (Vec2.getY v)

        normal =
            vec3
                ((elevationEast - elevationWest) / stepX)
                2
                ((elevationSouth - elevationNorth) / stepY)
                |> Vec3.normalize

        pos =
            vec3
                (Vec2.getX v)
                (z * options.height)
                (Vec2.getY v)
    in
    ( z, pos, normal )
