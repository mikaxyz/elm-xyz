module Scenes.Light exposing (init, sceneOptions)

import Material
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene, defaultScene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object


speed =
    48


init : Scene Material.Name
init =
    { defaultScene
        | graph =
            [ Graph
                (XYZMika.XYZ.Mesh.Cube.colorful 2 0.2 2
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -1 0)
                    |> Object.withOptionDragToRotateY
                )
                [ Graph
                    (XYZMika.XYZ.Mesh.Cube.gray 1 1 1
                        |> Object.withMesh
                        |> Object.withPosition (vec3 0 0.6 -0.5)
                        |> Object.withOptionDragToRotateX
                    )
                    [ Graph
                        (XYZMika.XYZ.Mesh.Cube.colorful 0.4 0.2 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0.6 0)
                        )
                        []
                    , Graph
                        (XYZMika.XYZ.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0.6 0 0)
                        )
                        []
                    , Graph
                        (XYZMika.XYZ.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 -0.6 0 0)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 -1 0 0))
                        )
                        []
                    , Graph
                        (XYZMika.XYZ.Mesh.Cube.colorful 0.4 0.4 0.2
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0 0.6)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 1))
                        )
                        []
                    , Graph
                        (XYZMika.XYZ.Mesh.Cube.colorful 0.4 0.4 0.2
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0 -0.6)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 -1))
                        )
                        []
                    ]
                ]
            , light Scene.lightPosition1
            , light Scene.lightPosition2
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 4) (vec3 0 0 0) (vec3 0 1 0)
    }


light p =
    Graph
        (XYZMika.XYZ.Mesh.Cube.colorful 0.02 0.02 0.02
            |> Object.withMesh
            |> Object.withPosition p
        )
        []


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }
