module Scenes.NormalMapping exposing (init)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { verticesIndexed : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    , normal : Texture
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    assets
        |> getAssets
        |> Maybe.map render
        |> Maybe.withDefault []
        |> Graph.graph (XYZMika.XYZ.Mesh.Cube.gray 0 0 0 |> Object.initWithTriangles)
        |> Scene.init
        |> Scene.withCameraPosition (vec3 -3 1.5 2.5)
        --|> Scene.withCameraPosition (vec3 -2 1 1.5)
        |> Scene.withCameraTarget (vec3 0 0.5 0)


render : Assets -> List (Graph (Object Material.Name))
render cube =
    [ pointLight 0.1 (vec3 5 5 -5) (vec3 0.85 0.95 1)
    , pointLight 0.8 (vec3 -3 3 10) (vec3 0.99 0.99 0.9)
    , pointLight 0.2 (vec3 -6 -3 -6) (vec3 0.99 0.99 0.99)
    , cube.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 0 0.55 0)
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withNormalMap cube.normal
        |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (2 * theta) Vec3.j)
        |> Object.withMaterialName Material.Advanced
        |> Graph.singleton
    ]


pointLight : Float -> Vec3 -> Vec3 -> Graph (Object materialId)
pointLight intensity position color =
    Object.light
        (Light.pointLight position
            |> Light.withIntensity intensity
            |> Light.withColorVec color
        )
        |> Graph.singleton


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map3
        Assets
        (AssetStore.verticesIndexed Asset.SneakerXyz assets)
        (AssetStore.texture Asset.SneakerDiffuse assets)
        (AssetStore.texture Asset.SneakerNormal assets)
