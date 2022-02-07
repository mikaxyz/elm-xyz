module Scenes.BrickWall exposing (init, sceneOptions)

import Asset
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree exposing (Tree)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { cubeVertices : List ( Vertex, Vertex, Vertex )
    , cube : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    , normal : Texture
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assets =
    assets
        |> getAssets
        |> Maybe.map render
        |> Maybe.withDefault []
        |> (\x ->
                Tree.tree
                    (Object.group "Some group")
                    [ pointLight 0.2 (Vec3.vec3 2 2 0) (vec3 0.1 0.3 1)
                    , pointLight 0.2 (Vec3.vec3 2 2 1) (vec3 0.8 0.8 0.3)
                    , pointLight 0.2 (Vec3.vec3 2 2 2) (vec3 0.8 0.1 0)
                    , pointLight 0.2 (Vec3.vec3 2 2 3) (vec3 0.5 0.1 1)
                    , Tree.tree
                        (Object.group "Some Bricks"
                            |> Object.withPosition (vec3 0 2 0)
                            |> Object.withOptionDragToRotateXY
                        )
                        x
                    ]
           )
        |> Scene.init
        |> Scene.withCamera { position = vec3 0 3 8, target = vec3 0 1 0 }


pointLight : Float -> Vec3 -> Vec3 -> Tree (Object materialId)
pointLight intensity position color =
    Object.light position
        (Light.pointLight (Vec3.vec3 0 0 0)
            |> Light.withIntensity intensity
            |> Light.withColorVec color
        )
        |> Tree.singleton


render : Assets -> List (Tree (Object Material.Name))
render assets =
    let
        indexedVerticesToTree :
            Vec3
            -> ( List Vertex, List ( Int, Int, Int ) )
            -> Tree (Object Material.Name)
        indexedVerticesToTree pos mesh =
            Tree.singleton
                (mesh
                    |> Object.initWithIndexedTriangles
                    |> Object.withPosition pos
                    |> Object.withNormalMap assets.normal
                    |> Object.withDiffuseMap assets.diffuse
                    |> Object.withMaterialName Material.Advanced
                )

        d1 =
            1

        d2 =
            d1
    in
    [ assets.cube |> indexedVerticesToTree (Vec3.vec3 -d1 0 0)
    , assets.cube |> indexedVerticesToTree (Vec3.vec3 0 -d2 0)
    , assets.cube |> indexedVerticesToTree (Vec3.vec3 0 0 0)
    , assets.cube
        |> indexedVerticesToTree (Vec3.vec3 0 d2 0)
        |> Tree.replaceChildren [ pointLight 0.4 (Vec3.vec3 0 1 0) (Vec3.vec3 1 1 1) ]
    , assets.cube |> indexedVerticesToTree (Vec3.vec3 d1 0 0)
    , assets.cube
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (Vec3.vec3 0 -2 0)
        |> Object.withRotation (Mat4.makeScale3 5 1 5)
        |> Object.withNormalMap assets.normal
        |> Object.withDiffuseMap assets.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Tree.singleton
    ]


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map4
        Assets
        (AssetStore.vertices Asset.Cube assets)
        (AssetStore.verticesIndexed Asset.Cube assets)
        (AssetStore.texture Asset.BrickWallDiffuse assets)
        (AssetStore.texture Asset.BrickWallNormal assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
