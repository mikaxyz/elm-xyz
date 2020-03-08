module Scenes.Textures exposing (init, sceneOptions)

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


type alias BallAssets =
    { vertices : List ( Vertex, Vertex, Vertex )
    , verticesIndexed : ( List Vertex, List ( Int, Int, Int ) )
    , mesh : Mesh Vertex
    , diffuse : Texture
    , normal : Texture
    }


type alias TreeAssets =
    { vertices : List ( Vertex, Vertex, Vertex )
    , mesh : Mesh Vertex
    , diffuse : Texture
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    Scene.init
        [ Graph
            (XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -6 -0.5 -6, vec3 6 0 6 )
                |> Object.initWithTriangles
                |> Object.withPosition (vec3 0 -0.5 0)
                |> Object.withOptionDragToRotateXY
                |> Object.withColor Color.blue
                |> Object.withMaterialName Material.Color
            )
            (Maybe.map2 render
                (getBallAssets assets)
                (getTreeAssets assets)
                |> Maybe.withDefault []
            )
        ]
        |> Scene.withCamera (Mat4.makeLookAt (vec3 0 0.5 3) (vec3 0 0.5 0) (vec3 0 1 0))


render : BallAssets -> TreeAssets -> List (Graph Material.Name)
render ball tree =
    [ ball.vertices
        |> Object.initWithTriangles
        |> Object.withDiffuseMap ball.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0.7 0 0)
        |> (\x -> Graph x [])
    , ball.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withDiffuseMap ball.diffuse
        |> Object.withNormalMap ball.normal
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 0 0 0)
        |> (\x -> Graph x [])
    , ball.vertices
        |> Object.initWithTriangles
        |> Object.withNormalMap ball.normal
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 -0.7 0 0)
        |> (\x -> Graph x [])
    , tree.vertices
        |> Object.initWithTriangles
        |> Object.withDiffuseMap tree.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 -2 0 -3)
        |> (\x -> Graph x [])
    , tree.vertices
        |> Object.initWithTriangles
        |> Object.withDiffuseMap tree.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Object.withPosition (vec3 2 0 -5)
        |> (\x -> Graph x [])
    , ball.vertices
        |> Object.initWithTriangles
        |> Object.withDiffuseMap ball.diffuse
        |> Object.withNormalMap ball.normal
        |> Object.withColor Color.yellow
        |> Object.withMaterialName Material.Advanced
        |> Object.withNormalMapIntensity 10.0
        |> Object.withPosition (vec3 1 0 -3)
        |> Object.withOptionRotationInTime
            (\theta ->
                let
                    r =
                        sin (theta * 30)

                    y =
                        abs r * 0.5
                in
                Mat4.makeTranslate3 0 y 0
            )
        |> (\x -> Graph x [])
    ]


getBallAssets : Store Asset.Obj Asset.Texture -> Maybe BallAssets
getBallAssets assets =
    Maybe.map5
        BallAssets
        (AssetStore.vertices Asset.Ball assets)
        (AssetStore.verticesIndexed Asset.Ball assets)
        (AssetStore.mesh Asset.Ball assets)
        (AssetStore.texture Asset.BallDiffuse assets)
        (AssetStore.texture Asset.BallNormal assets)


getTreeAssets : Store Asset.Obj Asset.Texture -> Maybe TreeAssets
getTreeAssets assets =
    Maybe.map3
        TreeAssets
        (AssetStore.vertices Asset.Tree assets)
        (AssetStore.mesh Asset.Tree assets)
        (AssetStore.texture Asset.TreeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
