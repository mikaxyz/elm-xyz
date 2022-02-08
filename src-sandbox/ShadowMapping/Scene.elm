module ShadowMapping.Scene exposing (graph, pointLightPosition)

import Color
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph as Scene
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object


pointLightPosition : Float -> Vec3
pointLightPosition theta =
    -- Initial camera position
    --vec3 0 3 4
    vec3 -2 2 0
        |> Mat4.transform (Mat4.makeRotate (2 * theta) (vec3 0 1 0))


graph : Maybe Texture -> { sneakers : ( List Vertex, List ( Int, Int, Int ) ) } -> Scene.Graph (Object.Object Material.Name)
graph lightMap { sneakers } =
    Scene.graph
        (Object.group "ROOT")
        [ Object.light (pointLightPosition 0.0)
            (Light.pointLight (vec3 0 0 0)
                |> Light.withIntensity 1.0
                --|> Light.withColor (Color.rgb 0.8 0.8 0.6)
                |> Light.withColor Color.white
            )

        --|> Object.withOptionRotationInTime
        --    (\theta ->
        --        Mat4.makeRotate (0.1 * theta) (vec3 0 1 0)
        --    )
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
            |> Object.withPosition (vec3 0 -0.1 0)

        --, XYZMika.XYZ.Mesh.Cube.colored Color.darkRed 1 1 1
        --    |> Object.initWithTriangles
        --    |> Object.withPosition (vec3 0 0 0)
        , sneakers
            |> Object.initWithIndexedTriangles
            |> Object.withPosition (vec3 0 0.55 0)

        --|> Object.withDiffuseMap sneakers.diffuse
        --|> Object.withNormalMap sneakers.normal
        --|> Object.withOptionRotationInTime (\theta -> Mat4.makeRotate (2 * theta) Vec3.j)
        --|> Object.withMaterialName Material.Advanced
        --|> Tree.singleton
        ]
