module XYZMika.XYZ.Scene exposing
    ( Options
    , Renderer
    , Scene
    , init
    , lightPosition1
    , lightPosition2
    , map
    , render
    , withCamera
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


directionalLight : Vec3
directionalLight =
    Vec3.fromRecord { x = 1, y = 0.7, z = 0.5 }


lightPosition1 =
    vec3 -4 3 3


lightPosition2 =
    vec3 -6 2 3.5


type Scene materialId
    = Scene
        { graph : List (Graph materialId)
        , camera : Mat4
        , cameraRotate : Mat4
        }


init : List (Graph materialId) -> Scene materialId
init graph =
    Scene
        { graph = graph
        , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 0 0) (vec3 0 1 0)
        , cameraRotate = Mat4.identity
        }


withCamera : Mat4 -> Scene materialId -> Scene materialId
withCamera x (Scene scene) =
    Scene { scene | camera = x }


map : (Graph materialId -> Graph materialId) -> Scene materialId -> Scene materialId
map f (Scene scene) =
    Scene { scene | graph = scene.graph |> List.map f }


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


type alias Renderer materialId uniforms =
    -- TODO: Move into Material.elm
    Maybe materialId
    -> Texture
    -> uniforms
    -> Object materialId
    -> Entity


render :
    Texture
    -> { a | width : Int, height : Int }
    -> Vec2
    -> Float
    -> Maybe Options
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
render defaultTexture viewport drag theta options (Scene scene) renderer =
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
        { sceneCamera = scene.camera
        , scenePerspective = options_.perspective aspectRatio
        , sceneMatrix = Mat4.identity
        }
        defaultTexture
        scene.graph
        renderer


renderGraph :
    Vec2
    -> Float
    -> Uniforms u
    -> Texture
    -> List (Graph materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta uniforms defaultTexture graph renderer =
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

                            sceneMatrix =
                                Object.rotation object_
                                    |> Mat4.mul (Mat4.makeTranslate (Object.position object_))
                                    |> Mat4.mul uniforms.sceneMatrix

                            entity uniforms_ =
                                object
                                    |> Object.materialName
                                    |> renderer
                                    |> (\r -> r defaultTexture uniforms_ object)
                        in
                        entity { uniforms | sceneMatrix = sceneMatrix }
                            :: renderGraph drag theta { uniforms | sceneMatrix = sceneMatrix } defaultTexture children renderer
            )
        |> List.concat
