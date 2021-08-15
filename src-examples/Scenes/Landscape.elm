module Scenes.Landscape exposing (init, sceneOptions)

import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree exposing (Tree)
import WebGL exposing (Mesh, Shader)
import XYZMika.Color as Color exposing (Color)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Generator.Perlin as Perlin
import XYZMika.XYZ.Mesh.Landscape
import XYZMika.XYZ.Mesh.Primitives
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
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

        normalBone : Vertex -> Tree (Object Material.Name)
        normalBone v =
            WebGL.lines [ ( v, { v | position = Vec3.add v.position v.normal } ) ]
                |> Object.init
                |> Tree.singleton

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

        bone : Vec3 -> Tree (Object Material.Name)
        bone v =
            XYZMika.XYZ.Mesh.Primitives.bone Color.red Color.green 0.05 (Vec3.getY (Vec3.add (vec3 0 1 0) v))
                |> WebGL.triangles
                |> Object.init
                |> Object.withPosition (Vec3.setY -1 v)
                |> Tree.singleton

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
    Scene.init { gizmoMaterial = Material.Simple }
        (landscape
            |> (\( v, vmap ) -> WebGL.indexedTriangles v vmap)
            |> Object.init
            |> Object.withMaterialName Material.Advanced
            --                |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (4 * theta) (vec3 0 1 0))
            |> Object.withOptionDragToRotateXY
            |> (\obj -> Tree.tree obj helpers)
        )
        |> Scene.withCameraPosition (vec3 0 4 7)


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }
