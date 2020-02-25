module Scenes.Textures exposing (init, renderBall, sceneOptions)

import Asset
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene exposing (Options, Scene, defaultScene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    { defaultScene
        | graph =
            [ Graph
                (XYZMika.XYZ.Mesh.Cube.gray 12 0.2 12
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -0.5 0)
                    |> Object.withOptionDragToRotateXY
                )
                (getAssets assets |> Maybe.map renderBall |> Maybe.withDefault [])
            ]
        , camera = Mat4.makeLookAt (vec3 0 0.5 3) (vec3 0 0.5 0) (vec3 0 1 0)
    }


renderBall : Config -> List (Graph Material.Name)
renderBall config =
    [ config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0.7 0.1 0)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withNormalMap config.normal
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0 0.1 0)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withNormalMap config.normal
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 -0.7 0.1 0)
        |> (\x -> Graph x [])
    , config.treeMesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.treeDiffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0 0 -6)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withNormalMap config.normal
        |> Object.withNormalMapIntensity 10.0
        |> Object.withPosition (vec3 1 0.1 -5)
        |> Object.withOptionRotationInTime
            (\theta ->
                let
                    r =
                        sin (theta * 60)

                    y =
                        r
                in
                Mat4.makeTranslate3 0 (abs y * 0.5) 0
            )
        |> (\x -> Graph x [])
    ]


type alias Config =
    { mesh : Mesh Vertex
    , diffuse : Texture
    , normal : Texture
    , treeMesh : Mesh Vertex
    , treeDiffuse : Texture
    }


getAssets : Store Asset.Obj Asset.Texture -> Maybe Config
getAssets assets =
    Maybe.map5
        Config
        (AssetStore.mesh Asset.Ball assets)
        (AssetStore.texture Asset.BallDiffuse assets)
        (AssetStore.texture Asset.BallNormal assets)
        (AssetStore.mesh Asset.Tree assets)
        (AssetStore.texture Asset.TreeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
