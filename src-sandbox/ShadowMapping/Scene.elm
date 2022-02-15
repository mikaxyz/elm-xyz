module ShadowMapping.Scene exposing (graph)

import Color
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight
import XYZMika.XYZ.Scene.Object as Object


type alias Assets =
    { mesh : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    , normal : Texture
    , carpetDiffuse : Texture
    , carpetNormal : Texture
    }


graph : Assets -> Graph (Object.Object Material.Name)
graph assets =
    [ Object.spotLight
        (SpotLight.light (vec3 2 4 -3) 45
            |> SpotLight.withShadowMap
                { resolution = 800
                , near = 0.01
                , far = 100
                }
        )
    , Object.spotLight
        (SpotLight.light (vec3 -3 3 1) 30
            |> SpotLight.withShadowMap
                { resolution = 800
                , near = 0.01
                , far = 100
                }
        )
    , assets.mesh
        |> Object.initWithIndexedTriangles
        |> Object.withPosition (vec3 0 0.55 0)
        |> Object.withDiffuseMap assets.diffuse
        |> Object.withNormalMap assets.normal

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
        ++ floor 3.0 assets
        |> Graph.shallow (Object.group "ROOT")


floor tileWidth assets =
    List.range -2 2
        |> List.concatMap
            (\x ->
                List.range -2 2
                    |> List.map
                        (\y ->
                            XYZMika.XYZ.Mesh.Cube.colored Color.darkGreen tileWidth 0.2 tileWidth
                                |> Object.initWithTriangles
                                |> Object.withDiffuseMap assets.carpetDiffuse
                                |> Object.withNormalMap assets.carpetNormal
                                |> Object.withPosition (vec3 (toFloat x * tileWidth) -0.1 (toFloat y * tileWidth))
                        )
            )
