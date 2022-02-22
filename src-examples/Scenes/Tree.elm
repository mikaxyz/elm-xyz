module Scenes.Tree exposing (init)

import Material
import Math.Vector3 exposing (vec3)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Tree exposing (tree)
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph
import XYZMika.XYZ.Scene.Object as Object exposing (Object)



--


init : Scene objectId Material.Name
init =
    Graph.graph
        (XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.5 -0.2 -0.5, vec3 0.5 0.2 0.5 )
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.7 0)
        )
        [ Graph.graph
            (XYZMika.XYZ.Mesh.Cube.colorful 0.01 0.01 0.01
                |> Object.initWithTriangles
                |> Object.withPosition (vec3 0 0.1 0)
            )
            (tree 8 0)
        ]
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 1 2.5)
