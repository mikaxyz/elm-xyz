module Scenes.NormalMapping exposing (ObjectId, init, modifiers)

import Asset
import Color
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore exposing (Store)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Assets =
    { verticesIndexed : ( List Vertex, List ( Int, Int, Int ) )
    , diffuse : Texture
    , normal : Texture
    , carpetDiffuse : Texture
    , carpetNormal : Texture
    }


type ObjectId
    = Sneaker


modifiers : Float -> (ObjectId -> a) -> List (Scene.Modifier a b)
modifiers theta sceneObject =
    [ Scene.ObjectModifier (sceneObject Sneaker)
        (Object.map
            (\x -> { x | rotation = Mat4.makeRotate (2 * theta) Vec3.j })
        )
    ]


init : (ObjectId -> a) -> Store Asset.Obj Asset.Texture -> Scene a Material.Name
init objectId assets =
    assets
        |> getAssets
        |> Maybe.map (render objectId)
        |> Maybe.withDefault (Graph.singleton (XYZMika.XYZ.Mesh.Cube.gray 0 0 0 |> Object.initWithTriangles))
        |> Scene.init
        |> Scene.withCameraPosition (vec3 0 1 3)
        |> Scene.withCameraTarget (vec3 0 0.5 0)


render : (ObjectId -> id) -> Assets -> Graph (Object id Material.Name)
render objectId assets =
    Graph.shallow
        (Object.group "STUFF")
        ((assets.verticesIndexed
            |> Object.objectObjectWithIndexedTriangles (objectId Sneaker)
            |> Object.withPosition (vec3 0 0.55 0)
            |> Object.withDiffuseMap assets.diffuse
            |> Object.withNormalMap assets.normal
            |> Object.withMaterialName Material.Advanced
         )
            :: Object.spotLight
                (SpotLight.light (vec3 -2 5 -2) 55
                    |> SpotLight.withColor (Color.rgb 0.8 0.9 0.99)
                    |> SpotLight.withTarget (vec3 0 0 0)
                    |> SpotLight.withIntensity 0.2
                    |> SpotLight.withShadowMap
                        { resolution = 800
                        , near = 0.01
                        , far = 100
                        }
                )
            :: Object.spotLight
                (SpotLight.light (vec3 4 6 1) 55
                    |> SpotLight.withColor (Color.rgb 0.99 0.9 0.8)
                    |> SpotLight.withTarget (vec3 0 0 0)
                    |> SpotLight.withIntensity 0.3
                    |> SpotLight.withShadowMap
                        { resolution = 800
                        , near = 0.01
                        , far = 100
                        }
                )
            :: Object.spotLight
                (SpotLight.light (vec3 -4 4 6) 55
                    |> SpotLight.withColor (Color.rgb 0.99 0.96 0.98)
                    |> SpotLight.withTarget (vec3 0 0 0)
                    |> SpotLight.withIntensity 0.4
                    |> SpotLight.withShadowMap
                        { resolution = 800
                        , near = 0.01
                        , far = 100
                        }
                )
            :: floor 3.0 assets
        )


floor : Float -> Assets -> List (Object id Material.Name)
floor tileWidth assets =
    List.range -2 2
        |> List.concatMap
            (\x ->
                List.range -2 2
                    |> List.map
                        (\y ->
                            XYZMika.XYZ.Mesh.Cube.colored Color.yellow tileWidth 0.21 tileWidth
                                |> Object.initWithTriangles
                                |> Object.withDiffuseMap assets.carpetDiffuse
                                |> Object.withNormalMap assets.carpetNormal
                                |> Object.withPosition (vec3 (toFloat x * tileWidth) -0.1 (toFloat y * tileWidth))
                                |> Object.withMaterialName Material.Advanced
                        )
            )


getAssets : Store Asset.Obj Asset.Texture -> Maybe Assets
getAssets assets =
    Maybe.map5
        Assets
        (AssetStore.verticesIndexed Asset.SneakerXyz assets)
        (AssetStore.texture Asset.SneakerDiffuse assets)
        (AssetStore.texture Asset.SneakerNormal assets)
        (AssetStore.texture Asset.CarpetDiffuse assets)
        (AssetStore.texture Asset.CarpetNormal assets)
