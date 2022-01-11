module Scenes.Animals exposing (init, sceneOptions)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { deer : List ( Vertex, Vertex, Vertex )
    , wolf : List ( Vertex, Vertex, Vertex )
    , cat : List ( Vertex, Vertex, Vertex )

    --, monkey : List ( Vertex, Vertex, Vertex )
    }


init : Store Asset.Obj Asset.Texture -> Scene Material.Name
init assetStore =
    Maybe.map3 Assets
        (AssetStore.vertices Asset.Deer assetStore)
        (AssetStore.vertices Asset.Wolf assetStore)
        (AssetStore.vertices Asset.Cat assetStore)
        --(AssetStore.vertices Asset.Monkey assetStore)
        |> Maybe.map
            (\assets ->
                Tree.tree (floor Color.lightGreen)
                    [ assets.deer
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 1 0.5 0.5)
                        |> Object.withPosition (Vec3.vec3 -1 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced
                        |> Tree.singleton
                    , assets.wolf
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 0.5 1 1)
                        |> Object.withPosition (Vec3.vec3 0 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced
                        |> Tree.singleton
                    , assets.cat
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 1 1 0.5)
                        |> Object.withPosition (Vec3.vec3 1 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced
                        |> Tree.singleton

                    --[ assets.monkey
                    --    |> Object.initWithTriangles
                    --    |> Object.withColor (Color.rgb 1 1 0)
                    --    |> Object.withPosition (Vec3.vec3 -2 3 0)
                    --    --|> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                    --    |> Object.withMaterialName Material.Advanced
                    --    |> Tree.singleton
                    ]
            )
        |> Maybe.withDefault (Tree.singleton (floor Color.darkCharcoal))
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 1 3)


floor : Color.Color -> Object Material.Name
floor color =
    XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -2 -0.5 -2, vec3 2 0 2 )
        |> Object.initWithTriangles
        |> Object.withColor color
        |> Object.withPosition (vec3 0 0.1 0)
        |> Object.withMaterialName Material.Advanced
        |> Object.withOptionDragToRotateXY


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 60 aspectRatio 0.01 100
        }