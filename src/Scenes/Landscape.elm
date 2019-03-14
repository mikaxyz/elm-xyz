module Scenes.Landscape exposing (init)

import DDD.Mesh.Cube
import DDD.Scene exposing (Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object



--


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph (DDD.Mesh.Cube.mesh 0.1 0.1 0.1 |> Object.withMesh) [] ]
    }
