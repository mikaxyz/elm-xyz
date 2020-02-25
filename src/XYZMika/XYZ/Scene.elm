module XYZMika.XYZ.Scene exposing
    ( Options
    , Scene
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


directionalLight : Vec3
directionalLight =
    Vec3.fromRecord { x = 1, y = 0.7, z = 0.5 }


lightPosition1 =
    vec3 -4 3 3


lightPosition2 =
    vec3 -6 2 3.5


type alias Scene materialId =
    { graph : List (Graph materialId)
    , camera : Mat4
    , cameraRotate : Mat4
    }


defaultScene : Scene materialId
defaultScene =
    { graph = []
    , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 0 0) (vec3 0 1 0)
    , cameraRotate = Mat4.identity
    }


type alias Options =
    { rotation : Float -> Mat4
    , translate : Float -> Mat4
    , perspective : Float -> Mat4
    }


defaultOptions : Options
defaultOptions =
    { rotation = always Mat4.identity
    , translate = always Mat4.identity
    , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
    }


render defaultTexture viewport drag theta options scene renderer =
    -- TODO: Fix Type annotation!!!
    --TODO: Remove defaultTexture. Require a texture in object if Advanced renderer?
    let
        options_ =
            options |> Maybe.withDefault defaultOptions

        aspectRatio =
            toFloat viewport.width / toFloat viewport.height
    in
    renderGraph
        drag
        theta
        { camera = scene.camera
        , perspective = options_.perspective aspectRatio
        , worldMatrix = Mat4.identity
        }
        defaultTexture
        scene.graph
        renderer


renderGraph drag theta uniforms defaultTexture graph renderer =
    -- TODO: Fix Type annotation!!!
    graph
        |> List.map
            (\g ->
                case g of
                    Graph object children ->
                        let
                            object_ =
                                object
                                    |> Object.rotationWithDrag drag
                                    |> Object.rotationInTime theta

                            worldMatrix =
                                Object.rotation object_
                                    |> Mat4.mul (Mat4.makeTranslate (Object.position object_))
                                    |> Mat4.mul uniforms.worldMatrix

                            entity uniforms_ =
                                object
                                    |> Object.materialName
                                    |> renderer
                                    |> (\r -> r defaultTexture uniforms_ object)
                        in
                        entity { uniforms | worldMatrix = worldMatrix }
                            :: renderGraph drag theta { uniforms | worldMatrix = worldMatrix } defaultTexture children renderer
            )
        |> List.concat
