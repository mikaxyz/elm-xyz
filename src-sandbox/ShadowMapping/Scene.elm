module ShadowMapping.Scene exposing (graph, graphForShadowMap, pointLightPosition)

import Color
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph as Scene
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object


pointLightPosition : Float -> Math.Vector3.Vec3
pointLightPosition theta =
    vec3 0 2 -3



--|> Mat4.transform (Mat4.makeRotate (0.8 * theta) (vec3 0 1 0))


graph : Maybe Texture -> Scene.Graph (Object.Object Material.Name)
graph lightMap =
    Scene.graph
        (Object.group "ROOT")
        [ Object.light (pointLightPosition 0.0)
            (Light.pointLight (vec3 0 0 0)
                |> Light.withIntensity 1.0
                --|> Light.withColor (Color.rgb 0.8 0.8 0.6)
                |> Light.withColor Color.white
            )
            |> Object.withOptionRotationInTime
                (\theta ->
                    Mat4.makeRotate (0.1 * theta) (vec3 0 1 0)
                )
        , case lightMap of
            Just texture ->
                XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen 4 4 0.2
                    |> Object.initWithTriangles
                    |> Object.withDiffuseMap texture
                    |> Object.withMaterialName Material.Advanced
                    |> Object.withPosition (vec3 0 2 -5)

            Nothing ->
                Object.group "MIRROR"
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen 10 0.2 10
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.11 0)
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkRed 1 1 1
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 0.5 0)
        ]


graphForShadowMap : Scene.Graph (Object.Object Material.Name)
graphForShadowMap =
    Scene.graph
        (Object.group "ROOT")
        [ XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen 10 0.2 10
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.11 0)

        --|> Object.withMaterialName Material.DepthMap
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkRed 1 1 1
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 0.5 0)

        --|> Object.withMaterialName Material.DepthMap
        ]
