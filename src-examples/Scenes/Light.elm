module Scenes.Light exposing (ObjectId(..), init, modifiers)

import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object


type ObjectId
    = Box
    | Gear


modifiers : Float -> (ObjectId -> a) -> List (Scene.Modifier a b)
modifiers theta sceneObject =
    [ Scene.ObjectModifier (sceneObject Box)
        (Object.map
            (\x -> { x | rotation = Mat4.rotate (sin theta * 3) Vec3.j x.rotation })
        )
    , Scene.ObjectModifier (sceneObject Gear)
        (Object.map
            (\x -> { x | rotation = Mat4.rotate (theta * 60) Vec3.j x.rotation })
        )
    ]


init : (ObjectId -> objectId) -> Scene objectId Material.Name
init objectId =
    Graph.graph
        (XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -1 -0.6 -1, vec3 1 -0.5 1 )
            |> Object.initWithTriangles
            |> Object.withMaterialName Material.Advanced
        )
        [ Object.light
            (Light.pointLight (vec3 3 5 3)
                |> Light.withIntensity 1.0
                |> Light.withColor Color.white
            )
            |> Graph.singleton
        , Graph.shallow
            (XYZMika.XYZ.Mesh.Cube.withBoundsAndColor Color.lightPurple ( vec3 -0.5 -0.5 -0.5, vec3 0.5 0.5 0.5 )
                |> Object.objectWithTriangles (objectId Box)
                |> Object.withMaterialName Material.Advanced
                |> Object.withPosition (vec3 0 0 0)
            )
            [ gear objectId
                |> Object.withPosition (vec3 0 0.6 0)
            , gear objectId
                |> Object.withPosition (vec3 0.6 0 0)
                |> Object.withRotation (Mat4.makeRotate (-0.5 * pi) Vec3.k)
            , gear objectId
                |> Object.withPosition (vec3 -0.6 0 0)
                |> Object.withRotation (Mat4.makeRotate (0.5 * pi) Vec3.k)
            , gear objectId
                |> Object.withPosition (vec3 0 0 0.6)
                |> Object.withRotation (Mat4.makeRotate (0.5 * pi) Vec3.i)
            , gear objectId
                |> Object.withPosition (vec3 0 0 -0.6)
                |> Object.withRotation (Mat4.makeRotate (-0.5 * pi) Vec3.i)
            ]
        ]
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 4 7)


gear objectId =
    XYZMika.XYZ.Mesh.Cube.withBoundsColorful ( vec3 -0.2 -0.1 -0.2, vec3 0.2 0.1 0.2 )
        |> Object.objectWithTriangles (objectId Gear)
        |> Object.withMaterialName Material.Advanced
