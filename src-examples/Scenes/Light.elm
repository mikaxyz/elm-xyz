module Scenes.Light exposing (init, sceneOptions)

import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import Tree
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Object as Object


speed =
    48


init : Scene Material.Name
init =
    Tree.tree
        (XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -1 -0.6 -1, vec3 1 -0.5 1 )
            |> Object.initWithTriangles
            |> Object.withMaterialName Material.Advanced
            --|> Object.withPosition (vec3 0 -0.5 0)
            |> Object.withOptionDragToRotateY
        )
        [ Object.pointLight 1.0 (vec3 0 3 -3) (vec3 1 1 1) |> Tree.singleton
        , Tree.tree
            (XYZMika.XYZ.Mesh.Cube.withBoundsAndColor Color.lightPurple ( vec3 -0.5 -0.5 -0.5, vec3 0.5 0.5 0.5 )
                |> Object.initWithTriangles
                |> Object.withMaterialName Material.Advanced
                |> Object.withPosition (vec3 0 0 0)
                |> Object.withOptionDragToRotateX
            )
            [ Tree.singleton
                (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.2 -0.1 -0.2, vec3 0.2 0.1 0.2 )
                    |> Object.initWithTriangles
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withPosition (vec3 0 0.6 0)
                    |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 1 0))
                )
            , Tree.singleton
                (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.1 -0.2 -0.2, vec3 0.1 0.2 0.2 )
                    |> Object.initWithTriangles
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withPosition (vec3 0.6 0 0)
                    |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 1 0 0))
                )
            , Tree.singleton
                (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.1 -0.2 -0.2, vec3 0.1 0.2 0.2 )
                    |> Object.initWithTriangles
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withPosition (vec3 -0.6 0 0)
                    |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 -1 0 0))
                )
            , Tree.singleton
                (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.2 -0.2 -0.1, vec3 0.2 0.2 0.1 )
                    |> Object.initWithTriangles
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withPosition (vec3 0 0 0.6)
                    |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 1))
                )
            , Tree.singleton
                (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.2 -0.2 -0.1, vec3 0.2 0.2 0.1 )
                    |> Object.initWithTriangles
                    |> Object.withPosition (vec3 0 0 -0.6)
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 -1))
                )
            ]
        ]
        |> Scene.init { gizmoMaterial = Material.Simple }
        |> Scene.withCameraPosition (vec3 0 4 7)


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }
