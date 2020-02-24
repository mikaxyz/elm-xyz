module Scenes.Sandbox exposing (init, sceneOptions)

import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Tree exposing (tree)
import XYZMika.XYZ.Scene exposing (Options, Scene, defaultScene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object



--


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (XYZMika.XYZ.Mesh.Cube.colorful 1 0.2 1
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -0.7 0)
                    |> Object.withOptionDragToRotateXY
                )
                [ Graph
                    (XYZMika.XYZ.Mesh.Cube.colorful 0.01 0.01 0.01
                        |> Object.withMesh
                        |> Object.withPosition (vec3 0 0.1 0)
                    )
                    (tree 8 0)
                ]
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 2.5) (vec3 0 0 0) (vec3 0 1 0)
    }


sceneOptions : Maybe Options
sceneOptions =
    Nothing