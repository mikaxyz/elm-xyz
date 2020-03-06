module Scenes.NormalMapping exposing (init, sceneOptions)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object


type alias Assets =
    { vertices : List ( Vertex, Vertex, Vertex )
    , mesh : Mesh Vertex
    , diffuse : Texture
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    assets
        |> getAssets
        |> Maybe.map render
        |> Maybe.withDefault []
        |> Scene.init
        |> Scene.withCamera (Mat4.makeLookAt (vec3 0 0 3) (vec3 0 0 0) (vec3 0 1 0))


render : Assets -> List (Graph Material.Name)
render cube =
    [ cube.vertices
        |> Object.initWithTriangles
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> (\x -> Graph x [])
    ]


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map3
        Assets
        (AssetStore.vertices Asset.Cube assets)
        (AssetStore.mesh Asset.Cube assets)
        (AssetStore.texture Asset.CubeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
