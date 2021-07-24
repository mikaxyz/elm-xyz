module Scenes.Sandbox exposing (init, sceneOptions)

import Material
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Tree exposing (tree)
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object



--


init : Scene Material.Name
init =
    Scene.init { gizmoMaterial = Material.Simple }
        [ Graph
            (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.5 -0.2 -0.5, vec3 0.5 0.2 0.5 )
                |> Object.initWithTriangles
                |> Object.withPosition (vec3 0 -0.7 0)
                |> Object.withOptionDragToRotateXY
            )
            [ Graph
                (XYZMika.XYZ.Mesh.Cube.colorful 0.01 0.01 0.01
                    |> Object.init
                    |> Object.withPosition (vec3 0 0.1 0)
                )
                (tree 8 0)
            ]
        ]
        |> Scene.withCamera (Mat4.makeLookAt (vec3 0 1 2.5) (vec3 0 0 0) (vec3 0 1 0))


sceneOptions : Maybe Options
sceneOptions =
    Nothing
