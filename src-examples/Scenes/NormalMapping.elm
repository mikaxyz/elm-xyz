module Scenes.NormalMapping exposing (init, sceneOptions)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree exposing (Tree)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { verticesIndexed : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    assets
        |> getAssets
        |> Maybe.map render
        |> Maybe.withDefault []
        |> (\x -> Tree.tree (XYZMika.XYZ.Mesh.Cube.gray 0 0 0 |> Object.init) x)
        |> Scene.init { gizmoMaterial = Material.Simple }
        |> Scene.withCameraPosition (vec3 0 0 4.5)


render : Assets -> List (Tree (Object Material.Name))
render cube =
    let
        positionHandle size v =
            XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -size -size -size, vec3 size size size )
                |> Object.initWithTriangles
                |> Object.withPosition v

        normalBone : Vertex -> Tree (Object Material.Name)
        normalBone v =
            WebGL.lines
                [ ( v, { v | position = Vec3.add v.position v.normal } )

                --, ( v, { v | position = Vec3.add v.position v.tangent } )
                --, ( v, { v | position = Vec3.add v.position v.biTangent } )
                ]
                |> Object.init
                --|> Object.withPosition v.position
                |> (\obj -> Tree.tree obj [ Tree.singleton (positionHandle 0.02 v.position) ])

        normalGuides : List (Tree (Object Material.Name))
        normalGuides =
            cube.verticesIndexed
                |> Tuple.first
                |> List.map
                    (\v ->
                        { v
                            | color = vec3 0.8 0.2 0.2
                            , normal = Vec3.scale 0.2 v.normal
                        }
                    )
                |> List.map normalBone

        wireframeTri ( v1, v2, v3 ) =
            WebGL.lines [ ( v1, v2 ), ( v2, v3 ), ( v3, v1 ) ]
                |> Object.init

        wireframe : List ( Vertex, Vertex, Vertex ) -> List (Object Material.Name)
        wireframe triangles =
            triangles
                |> List.map wireframeTri
    in
    [ positionHandle 0.02 (Vec3.vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> (\x -> Tree.tree x normalGuides)

    --, positionHandle 0.02 (Vec3.vec3 0 0 0)
    --    |> Object.withOptionDragToRotateXY
    --    |> (\x -> Graph x (cube.vertices |> wireframe |> List.map objectToGraph))
    , cube.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Tree.singleton
    , cube.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 -1.5 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Tree.singleton
    , cube.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 1.5 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withColor Color.grey
        |> Object.withMaterialName Material.Advanced
        |> Tree.singleton
    ]


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map2
        Assets
        (AssetStore.verticesIndexed Asset.UvCube assets)
        (AssetStore.texture Asset.UvCubeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
