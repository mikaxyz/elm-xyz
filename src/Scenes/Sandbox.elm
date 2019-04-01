module Scenes.Sandbox exposing (init, sceneOptions)

import DDD.Mesh.Cube
import DDD.Mesh.Tree exposing (tree)
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import WebGL



--


init : Scene
init =
    { defaultScene
        | graph =
            tree 8 0
                ++ cubes 4 0.1
                ++ cubes 3 0.075
                ++ cubes 2 0.05
                ++ cubes 1 0.025
    }


sceneOptions : Maybe Options
sceneOptions =
    Nothing


cubes : Float -> Float -> List Graph
cubes r w =
    List.range 0 (round (4 / w))
        |> List.map toFloat
        |> List.map
            (\i ->
                Graph
                    (DDD.Mesh.Cube.colorful w 0.1 w
                        |> Object.withMesh
                        |> Object.withPosition (vec3 (r * (i / (4 / w))) (-i * (w * w) * r) 0)
                        |> Object.withRotation (Mat4.makeRotate (Basics.pi * w * i) (vec3 0 1 0))
                    )
                    []
            )
