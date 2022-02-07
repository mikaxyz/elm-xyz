module ShadowMapping.Scene exposing (graph)

import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph as Scene
import XYZMika.XYZ.Scene.Object as Object


graph : Scene.Graph (Object.Object Material.Name)
graph =
    Scene.graph
        (Object.group "ROOT")
        [ XYZMika.XYZ.Mesh.Cube.gray 1 1 1
            |> Object.initWithTriangles
            |> Object.withMaterialName Material.Advanced
        ]
