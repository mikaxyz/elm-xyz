module Scenes.Landscape exposing (init)

import Material
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.Color as Color exposing (Color)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Generator.Perlin as Perlin
import XYZMika.XYZ.Mesh.Landscape
import XYZMika.XYZ.Mesh.Primitives
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


elevation x y =
    let
        seed =
            42

        freq =
            0.5

        e1 =
            Perlin.value2d { seed = seed, freq = freq } x y
    in
    e1
        + (0.5 * e1 * max e1 0 * Perlin.value2d { seed = seed, freq = 3 * freq } x y)
        + (0.5 * e1 * max e1 0 * Perlin.value2d { seed = seed, freq = 10 * freq } x y)


init : Scene Material.Name
init =
    let
        divisions =
            31

        color height =
            let
                normalized =
                    height + 1 / 2
            in
            vec3
                normalized
                1
                normalized

        landscape : ( List Vertex, List ( Int, Int, Int ) )
        landscape =
            XYZMika.XYZ.Mesh.Landscape.simple
                { divisions = divisions
                , width = 3
                , length = 3
                , height = 1
                , color = color
                , elevation = elevation
                }

        normalBone : Vertex -> Graph (Object Material.Name)
        normalBone v =
            [ ( v, { v | position = Vec3.add v.position v.normal } ) ]
                |> Object.initWithLines
                |> Graph.singleton

        normalGuides =
            landscape
                |> Tuple.first
                |> List.map
                    (\v ->
                        { v
                            | color = vec3 0.2 0.2 0.2
                            , normal = Vec3.scale 0.2 v.normal
                        }
                    )
                |> List.map normalBone

        bone : Vec3 -> Graph (Object Material.Name)
        bone v =
            XYZMika.XYZ.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY (Vec3.add (vec3 0 1 0) v))
                |> Object.initWithTriangles
                |> Object.withPosition (Vec3.setY -1 v)
                |> Graph.singleton

        elevationBones density =
            landscape
                |> Tuple.first
                |> List.indexedMap
                    (\i v ->
                        if modBy density i == 0 && modBy density (i // divisions) == 0 then
                            Just v.position

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.map bone

        helpers =
            normalGuides ++ elevationBones 4
    in
    Scene.init
        (landscape
            |> Object.initWithIndexedTriangles
            |> Object.withMaterialName Material.Advanced
            --                |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (4 * theta) (vec3 0 1 0))
            |> Object.withOptionDragToRotateXY
            |> (\obj -> Graph.graph obj helpers)
        )
        |> Scene.withCameraPosition (vec3 0 4 7)
