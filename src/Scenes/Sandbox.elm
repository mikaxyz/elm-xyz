module Scenes.Sandbox exposing (init)

import DDD.Mesh.Cube
import DDD.Mesh.Tree exposing (tree)
import DDD.Scene exposing (Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)



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


cubes : Float -> Float -> List Graph
cubes r w =
    List.range 0 (round (4 / w))
        |> List.map toFloat
        |> List.map
            (\i ->
                Graph
                    { position = vec3 (r * (i / (4 / w))) (-i * (w * w) * r) 0
                    , rotation = Mat4.makeRotate (Basics.pi * w * i) (vec3 0 1 0)
                    , mesh = DDD.Mesh.Cube.mesh w 0.01 w
                    }
                    []
            )
