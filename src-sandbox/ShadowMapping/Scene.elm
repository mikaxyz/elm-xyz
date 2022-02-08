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


type alias Assets =
    { mesh : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    , normal : Texture
    }


pointLightPosition : Float -> Vec3
pointLightPosition theta =
    -- Initial camera position
    --vec3 0 3 4
    vec3 -3 4 2



--|> Mat4.transform (Mat4.makeRotate (5 * theta) (vec3 0 1 0))


graph : Float -> Maybe Texture -> Assets -> Scene.Graph (Object.Object Material.Name)
graph theta lightMap assets =
    Scene.graph
        (Object.group "ROOT")
        [ Object.light (pointLightPosition theta)
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
        , XYZMika.XYZ.Mesh.Cube.colored Color.white 0.05 0.05 0.05
            |> Object.initWithTriangles
            |> Object.withMaterialName Material.Color
            |> Object.withPosition (pointLightPosition theta |> Vec3.add (vec3 0 0.5 0))
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen 10 0.2 10
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.1 0)
        , assets.mesh
            |> Object.initWithIndexedTriangles
            |> Object.withPosition (vec3 0 0.55 0)
            |> Object.withDiffuseMap assets.diffuse
            |> Object.withNormalMap assets.normal
            |> Object.withRotation (Mat4.makeRotate (10 * theta) (vec3 0 1 0))

        -- Blocks
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkRed 0.5 1 0.5
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 -2 0.5 -1)
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkBlue 1 0.5 0.5
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 1.6 0.25 -1.5)
            |> Object.withRotation (Mat4.makeRotate (0.3 * pi) (vec3 0 1 0))
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkYellow 1 0.5 0.5
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 2.5 0.25 -1.2)
            |> Object.withRotation (Mat4.makeRotate (0.8 * pi) (vec3 0 1 0))
        , XYZMika.XYZ.Mesh.Cube.colored Color.darkRed 1 0.5 0.5
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 2.1 0.75 -1.3)
            |> Object.withRotation (Mat4.makeRotate (1.5 * pi) (vec3 0 1 0))
        ]
