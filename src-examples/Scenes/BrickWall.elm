module Scenes.BrickWall exposing (init, sceneOptions)

import Asset
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Gizmo as Gizmo
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
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
                Graph.init
                    (positionHandle 0.01 (vec3 0 0 0)
                        |> Object.withOptionDragToRotateXY
                    )
                    |> Graph.withChildren x
           )
        |> Scene.init { gizmoMaterial = Material.Simple }
        |> Scene.withCameraPosition (vec3 0 0 4)


positionHandle : Float -> Vec3 -> Object materialId
positionHandle size v =
    XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -size -size -size, vec3 size size size )
        |> Object.initWithTriangles
        |> Object.withPosition v


render : Assets -> List (Graph (Object Material.Name))
render assets =
    let
        normalGuide : Vertex -> Object materialId
        normalGuide v =
            let
                offset =
                    v.normal |> Vec3.normalize |> Vec3.scale 0.02

                vertexOffset : Vec3 -> Vertex -> Vertex
                vertexOffset offset_ vertex_ =
                    { vertex_ | position = vertex_.position |> Vec3.add offset_ }

                ( v1, v2, v3 ) =
                    ( v |> Vertex.withColor (vec3 1 0 0) |> vertexOffset offset
                    , v |> Vertex.withColor (vec3 0 1 0) |> vertexOffset offset
                    , v |> Vertex.withColor (vec3 0 0 1) |> vertexOffset offset
                    )

                biTangent =
                    Vec3.cross v.tangent v.normal

                scale =
                    0.2
            in
            WebGL.lines
                [ ( v1, { v1 | position = Vec3.add v1.position (Vec3.scale scale v1.normal) } )
                , ( v2, { v2 | position = Vec3.add v2.position (Vec3.scale scale v2.tangent) } )
                , ( v3, { v3 | position = Vec3.add v3.position (Vec3.scale scale biTangent) } )
                ]
                |> Object.init

        normalGuides : List ( Int, Vertex ) -> List (Graph (Object Material.Name))
        normalGuides vs =
            vs
                |> List.map
                    (\( i, v ) ->
                        { v
                            | color = vec3 0.9 0.9 0.9
                            , normal = v.normal
                            , tangent = v.tangent
                        }
                    )
                |> List.map (\v -> Graph.init (normalGuide v))

        addNormalGuides : ( List Vertex, List ( Int, Int, Int ) ) -> Graph (Object Material.Name) -> Graph (Object Material.Name)
        addNormalGuides mesh graph =
            graph |> Graph.mapChildren (\children -> children ++ (Tuple.first mesh |> List.indexedMap Tuple.pair |> normalGuides))

        wireframeTri : ( Vertex, Vertex, Vertex ) -> Object materialId
        wireframeTri ( v1, v2, v3 ) =
            WebGL.lines [ ( v1, v2 ), ( v2, v3 ), ( v3, v1 ) ]
                |> Object.init

        wireframe : List ( Vertex, Vertex, Vertex ) -> List (Object Material.Name)
        wireframe triangles =
            triangles
                |> List.map wireframeTri

        gizmo : Vertex -> Object materialId
        gizmo v =
            Gizmo.axis
                |> Object.init
                |> Object.withPosition v.position

        normalGizmos : List Vertex -> List (Graph (Object Material.Name))
        normalGizmos vs =
            vs
                |> List.map (\v -> Graph.init (gizmo v))

        objectToGraphWithNormalGizmo : Vec3 -> Material.Name -> ( List Vertex, List ( Int, Int, Int ) ) -> Graph (Object Material.Name)
        objectToGraphWithNormalGizmo pos material mesh =
            --mesh
            --    |> Object.initWithIndexedTriangles
            --    |> Object.withPosition pos
            --    |> Object.withNormalMap assets.normal
            --    |> Object.withMaterialName material
            --    |> objectToGraph
            --
            --mesh
            --    |> Object.initWithIndexedTriangles
            --
            --
            --
            --positionHandle 1.0 pos
            --    --|> Object.withPosition pos
            --    |> Object.withNormalMap assets.normal
            --    |> Object.withMaterialName material
            --    |> objectToGraph
            --    --|> Graph.map ((++) (normalGizmos <| Tuple.first mesh))
            --    --|> Graph.map ((++) (Tuple.first mesh |> List.indexedMap Tuple.pair |> normalGuides))
            --    |> Graph.map
            --        (\children ->
            --            Graph
            --                (mesh
            --                    |> Object.initWithIndexedTriangles
            --                    |> Object.withPosition pos
            --                    --|> Object.withNormalMap assets.normal
            --                    |> Object.withDiffuseMap assets.diffuse
            --                    |> Object.withMaterialName material
            --                 --|> objectToGraph
            --                )
            --                []
            --                :: children
            --        )
            Graph.init
                (mesh
                    |> Object.initWithIndexedTriangles
                    |> Object.withPosition pos
                    |> Object.withNormalMap assets.normal
                    |> Object.withDiffuseMap assets.diffuse
                    |> Object.withMaterialName material
                 --|> Object.withOptionDragToRotateXY
                 --|> objectToGraph
                )
                |> Graph.traverse (addNormalGuides mesh)

        --|> Graph.fmap ((++) (Tuple.first mesh |> List.indexedMap Tuple.pair |> normalGuides))
        --|> Graph.map ((++) (normalGizmos <| Tuple.first mesh))
        --|> Graph.mapChildren ((++) (normalGizmos <| Tuple.first mesh))
        --|> Graph.mapChildren ((++) (normalGuides <| Tuple.first mesh))
        d1 =
            1

        d2 =
            d1
    in
    [ assets.cube
        |> objectToGraphWithNormalGizmo (Vec3.vec3 -d1 0 0) Material.Advanced
    , assets.cube
        |> objectToGraphWithNormalGizmo (Vec3.vec3 0 -d2 0) Material.Advanced
    , assets.cube
        |> objectToGraphWithNormalGizmo (Vec3.vec3 0 0 0) Material.Advanced
    , assets.cube
        |> objectToGraphWithNormalGizmo (Vec3.vec3 0 d2 0) Material.Advanced
    , assets.cube
        |> objectToGraphWithNormalGizmo (Vec3.vec3 d1 0 0) Material.Advanced
    , assets.cube
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (Vec3.vec3 0 -2 0)
        |> Object.withRotation (Mat4.makeScale3 5 1 5)
        |> Object.withNormalMap assets.normal
        |> Object.withDiffuseMap assets.diffuse
        |> Object.withMaterialName Material.Advanced
        |> Graph.init
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
