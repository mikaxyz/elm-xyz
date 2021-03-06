module XYZMika.GridWorld exposing
    ( GridWorld
    , chunkSize
    , generate
    , generateChunks
    , geometry
    , gridFromCoord
    , init
    , withGenerator
    )

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import WebGL exposing (Mesh)


chunkSize =
    96


type GridWorld attr
    = GridWorld (MeshGenerator attr) (TerrainRepo attr)


type alias TerrainRepo attr =
    { lofi : Terrain attr
    , hifi : Terrain attr
    }


type alias Terrain attr =
    Dict ( Int, Int ) (Mesh attr)


type MeshGenerator attr
    = Generator (( Int, Int ) -> ( Vec2, Vec2 ) -> Mesh attr)


init : MeshGenerator attr -> GridWorld attr
init generator =
    GridWorld generator (TerrainRepo Dict.empty Dict.empty)


withGenerator : (( Int, Int ) -> ( Vec2, Vec2 ) -> Mesh attr) -> MeshGenerator attr
withGenerator generator =
    Generator generator


generateChunks : ( Int, Int ) -> ( Int, Int ) -> GridWorld attr -> GridWorld attr
generateChunks ( x1, y1 ) ( x2, y2 ) gridWorld =
    generateChunks_ ( x1, y1 ) ( x2, y2 ) ( 0, 0 ) gridWorld


generateChunks_ : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> GridWorld attr -> GridWorld attr
generateChunks_ ( x1, y1 ) ( x2, y2 ) ( xc, yc ) gridWorld =
    let
        world =
            generate ( x1 + xc, y1 + yc ) gridWorld
    in
    if x1 + xc == x2 && y1 + yc == y2 then
        world

    else if x1 + xc == x2 then
        generateChunks_ ( x1, y1 ) ( x2, y2 ) ( 0, yc + 1 ) world

    else
        generateChunks_ ( x1, y1 ) ( x2, y2 ) ( xc + 1, yc ) world


geometry : GridWorld attr -> List ( ( Float, Float ), Mesh attr )
geometry (GridWorld _ repo) =
    repo.lofi
        |> Dict.toList
        |> List.map
            (\( ( x, y ), mesh ) ->
                ( ( toFloat x * chunkSize
                  , toFloat y * chunkSize
                  )
                , mesh
                )
            )


gridFromCoord : Vec2 -> ( Int, Int )
gridFromCoord v =
    ( round (Vec2.getX v / chunkSize)
    , round (Vec2.getY v / chunkSize)
    )


generate : ( Int, Int ) -> GridWorld attr -> GridWorld attr
generate ( x, y ) (GridWorld generator repo) =
    ( x, y )
        :: (coordsAround 8 0 [] |> List.map (\( x_, y_ ) -> ( x_ + x, y_ + y )))
        |> List.foldl (\c acc -> generateTerrainAt c generator acc) repo.lofi
        |> (\terrain -> GridWorld generator { repo | lofi = terrain })


generateTerrainAt : ( Int, Int ) -> MeshGenerator attr -> Terrain attr -> Terrain attr
generateTerrainAt ( x, y ) (Generator generator) terrain =
    let
        chunk : Float -> Float -> Mesh attr
        chunk x1 y1 =
            generator
                ( x, y )
                ( vec2 ((x1 * chunkSize) - chunkSize * 0.5) ((y1 * chunkSize) + chunkSize * 0.5)
                , vec2 ((x1 * chunkSize) + chunkSize * 0.5) ((y1 * chunkSize) - chunkSize * 0.5)
                )
    in
    case terrain |> Dict.get ( x, y ) of
        Just _ ->
            terrain

        Nothing ->
            terrain
                |> Dict.insert ( x, y ) (chunk (toFloat x) (toFloat y))


coordsAround : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
coordsAround n i coords =
    let
        value =
            toFloat i / toFloat n

        theta =
            degrees (360 * value)

        c =
            fromPolar ( 1, theta )
                |> Tuple.mapFirst round
                |> Tuple.mapSecond round
    in
    if i == (n - 1) then
        c :: coords

    else
        coordsAround n (i + 1) (c :: coords)
