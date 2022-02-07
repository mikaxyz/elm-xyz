module ShadowMapping.Scene exposing (graph)

import Color
import Math.Vector3 exposing (vec3)
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph as Scene
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object


graph : Scene.Graph (Object.Object Material.Name)
graph =
    Scene.graph
        (Object.group "ROOT")
        [ Object.light (vec3 1 2 2)
            (Light.pointLight (vec3 0 0 0)
                |> Light.withIntensity 1.0
                |> Light.withColor (Color.rgb 0.8 0.8 0.6)
            )
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen 10 0.2 10
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.11 0)
            |> Object.withMaterialName Material.Advanced
        , XYZMika.XYZ.Mesh.Cube.gray 1 1 1
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 0.5 0)
            |> Object.withMaterialName Material.Advanced
        ]
