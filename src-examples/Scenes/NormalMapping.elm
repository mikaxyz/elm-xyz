module Scenes.NormalMapping exposing (init, sceneOptions)

import Asset
import Color
import Material
import Math.Vector3 exposing (Vec3, vec3)
import Tree exposing (Tree)
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
        |> Tree.tree (XYZMika.XYZ.Mesh.Cube.gray 0 0 0 |> Object.initWithTriangles)
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 0 4.5)


render : Assets -> List (Tree (Object Material.Name))
render cube =
    [ cube.verticesIndexed
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
