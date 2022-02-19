module Scenes.Animals exposing (init)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { deer : List ( Vertex, Vertex, Vertex )
    , wolf : List ( Vertex, Vertex, Vertex )
    , cat : List ( Vertex, Vertex, Vertex )

    --, monkey : List ( Vertex, Vertex, Vertex )
    }


init : Store Asset.Obj Asset.Texture -> Scene objectId Material.Name
init assetStore =
    Maybe.map3 Assets
        (AssetStore.vertices Asset.Deer assetStore)
        (AssetStore.vertices Asset.Wolf assetStore)
        (AssetStore.vertices Asset.Cat assetStore)
        --(AssetStore.vertices Asset.Monkey assetStore)
        |> Maybe.map
            (\assets ->
                Graph.shallow (floor Color.lightGreen)
                    [ assets.deer
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 1 0.5 0.5)
                        |> Object.withPosition (Vec3.vec3 -1 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced
                    , assets.wolf
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 0.5 1 1)
                        |> Object.withPosition (Vec3.vec3 0 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced
                    , assets.cat
                        |> Object.initWithTriangles
                        |> Object.withColor (Color.rgb 1 1 0.5)
                        |> Object.withPosition (Vec3.vec3 1 0 0)
                        |> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                        |> Object.withMaterialName Material.Advanced

                    --[ assets.monkey
                    --    |> Object.initWithTriangles
                    --    |> Object.withColor (Color.rgb 1 1 0)
                    --    |> Object.withPosition (Vec3.vec3 -2 3 0)
                    --    --|> Object.withRotation (Mat4.makeRotate -1.5 (vec3 0 1 0))
                    --    |> Object.withMaterialName Material.Advanced
                    ]
            )
        |> Maybe.withDefault (Graph.singleton (floor Color.darkCharcoal))
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 1 3)
        |> Scene.withPerspectiveProjection { fov = 60, near = 0.01, far = 100 }


floor : Color.Color -> Object objectId Material.Name
floor color =
    XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -2 -0.5 -2, vec3 2 0 2 )
        |> Object.initWithTriangles
        |> Object.withColor color
        |> Object.withPosition (vec3 0 0.1 0)
        |> Object.withMaterialName Material.Advanced
