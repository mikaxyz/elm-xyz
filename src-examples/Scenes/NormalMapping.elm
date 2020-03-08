module Scenes.NormalMapping exposing (init, sceneOptions)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { vertices : List ( Vertex, Vertex, Vertex )
    , verticesIndexed : ( List Vertex, List ( Int, Int, Int ) )
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
        |> Scene.withCamera (Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))


render : Assets -> List (Graph Material.Name)
render cube =
    let
        positionHandle size v =
            XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -size -size -size, vec3 size size size )
                |> Object.initWithTriangles
                |> Object.withPosition v

        normalBone : Vertex -> Graph Material.Name
        normalBone v =
            WebGL.lines
                [ ( v, { v | position = Vec3.add v.position v.normal } )

                --, ( v, { v | position = Vec3.add v.position v.tangent } )
                --, ( v, { v | position = Vec3.add v.position v.biTangent } )
                ]
                |> Object.init
                --|> Object.withPosition v.position
                |> (\obj -> Graph obj [ Graph (positionHandle 0.02 v.position) [] ])

        normalGuides : List (Graph Material.Name)
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

        objectToGraph x =
            Graph x []
    in
    [ positionHandle 0.02 (Vec3.vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> (\x -> Graph x normalGuides)
    , positionHandle 0.02 (Vec3.vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> (\x -> Graph x (cube.vertices |> wireframe |> List.map objectToGraph))
    , cube.verticesIndexed
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 0 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withMaterialName Material.Advanced
        |> objectToGraph
    , cube.vertices
        |> Object.initWithTriangles
        |> Object.withPosition (vec3 -1.5 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withDiffuseMap cube.diffuse
        |> Object.withMaterialName Material.Advanced
        |> objectToGraph
    , cube.vertices
        |> Object.initWithTriangles
        |> Object.withPosition (vec3 1.5 0 0)
        |> Object.withOptionDragToRotateXY
        |> Object.withColor Color.grey
        |> Object.withMaterialName Material.Advanced
        |> objectToGraph
    ]


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map4
        Assets
        (AssetStore.vertices Asset.Cube assets)
        (AssetStore.verticesIndexed Asset.Cube assets)
        (AssetStore.mesh Asset.Cube assets)
        (AssetStore.texture Asset.CubeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
