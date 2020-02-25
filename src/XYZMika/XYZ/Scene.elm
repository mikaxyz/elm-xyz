module XYZMika.XYZ.Scene exposing
    ( Options
    , Scene
    , defaultScene
    , lightPosition1
    , lightPosition2
    , render
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material as Material exposing (Material(..))
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


directionalLight =
    Vec3.fromRecord { x = 1, y = 0.7, z = 0.5 }


lightPosition1 =
    vec3 -4 3 3


lightPosition2 =
    vec3 -6 2 3.5


type alias Scene =
    { graph : List Graph
    , camera : Mat4
    , cameraRotate : Mat4
    }


defaultScene : Scene
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


render : Texture -> { width : Int, height : Int } -> Vec2 -> Float -> Maybe Options -> Scene -> List Entity
render defaultTexture viewport drag theta options scene =
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


renderGraph :
    Vec2
    -> Float
    -> { u | perspective : Mat4, camera : Mat4, worldMatrix : Mat4 }
    -> Texture
    -> List Graph
    -> List Entity
renderGraph drag theta uniforms defaultTexture graph =
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
                        in
                        entity defaultTexture { uniforms | worldMatrix = worldMatrix } object_
                            :: renderGraph drag theta { uniforms | worldMatrix = worldMatrix } defaultTexture children
            )
        |> List.concat


entity : Texture -> { u | perspective : Mat4, camera : Mat4, worldMatrix : Mat4 } -> Object -> Entity
entity defaultTexture uniforms object =
    case Object.materialName object of
        Material.Simple ->
            (\m ->
                WebGL.entity
                    (Material.vertexShader m)
                    (Material.fragmentShader m)
                    (Object.mesh object)
                    (Material.uniforms m)
            )
                (XYZMika.XYZ.Material.Simple.material uniforms)

        Material.Advanced ->
            (\m ->
                WebGL.entity
                    (Material.vertexShader m)
                    (Material.fragmentShader m)
                    (Object.mesh object)
                    (Material.uniforms m)
            )
                (XYZMika.XYZ.Material.Advanced.material
                    -- TODO: Alphabetize these
                    -- { aCamera, aWorldMatrix, aPerspective, texDiffuse, texHasDiffuse, etc }
                    { camera = uniforms.camera
                    , directionalLight = directionalLight
                    , diffuseMap = object |> Object.diffuseMapWithDefault defaultTexture
                    , hasDiffuseMap = Object.diffuseMap object /= Nothing
                    , hasNormalMap = Object.normalMap object /= Nothing
                    , normalMap = object |> Object.normalMapWithDefault defaultTexture
                    , perspective = uniforms.perspective
                    , worldMatrix = uniforms.worldMatrix
                    , normalMapIntensity = object |> Object.normalMapIntensityWithDefault 2.0
                    }
                )
