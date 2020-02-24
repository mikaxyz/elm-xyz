module Scenes.Light exposing (init, sceneOptions)

import DDD.Mesh.Cube
import DDD.Scene as Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)


speed =
    48


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.colorful 2 0.2 2
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -1 0)
                    |> Object.withOptionDragToRotateY
                )
                [ Graph
                    (DDD.Mesh.Cube.gray 1 1 1
                        |> Object.withMesh
                        |> Object.withPosition (vec3 0 0.6 -0.5)
                        |> Object.withOptionDragToRotateX
                    )
                    [ Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.2 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0.6 0)
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0.6 0 0)
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.2 0.4 0.4
                            |> Object.withMesh
                            |> Object.withPosition (vec3 -0.6 0 0)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 -1 0 0))
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.4 0.2
                            |> Object.withMesh
                            |> Object.withPosition (vec3 0 0 0.6)
                            |> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (speed * theta) (vec3 0 0 1))
                        )
                        []
                    , Graph
                        (DDD.Mesh.Cube.colorful 0.4 0.4 0.2
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
        (DDD.Mesh.Cube.colorful 0.02 0.02 0.02
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
