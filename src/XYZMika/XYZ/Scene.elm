module XYZMika.XYZ.Scene exposing
    ( Options
    , RenderOptions
    , Scene
    , direction
    , inDirection
    , init
    , map
    , render
    , withCamera
    , withRendererOptions
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material as Renderer exposing (Renderer)
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


direction : { right : Vec3, left : Vec3, up : Vec3, down : Vec3, forward : Vec3, backward : Vec3 }
direction =
    inDirection 10


inDirection : Float -> { right : Vec3, left : Vec3, up : Vec3, down : Vec3, forward : Vec3, backward : Vec3 }
inDirection d =
    { right = vec3 d 0 0
    , left = vec3 -d 0 0
    , up = vec3 0 d 0
    , down = vec3 0 -d 0
    , forward = vec3 0 0 -d
    , backward = vec3 0 0 d
    }


type Scene materialId
    = Scene
        { graph : List (Graph materialId)
        , camera : Mat4
        , cameraRotate : Mat4
        , rendererOptions : Renderer.Options
        }


withRendererOptions : Renderer.Options -> Scene materialId -> Scene materialId
withRendererOptions rendererOptions (Scene scene) =
    Scene { scene | rendererOptions = rendererOptions }


init : List (Graph materialId) -> Scene materialId
init graph =
    Scene
        { graph = graph
        , camera = Mat4.makeLookAt (vec3 0 3 4) (vec3 0 0 0) (vec3 0 1 0)
        , cameraRotate = Mat4.identity
        , rendererOptions = Renderer.defaultOptions
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


type alias RenderOptions =
    { showGeometry : Bool
    , showBoundingBoxes : Bool
    , showBoundingBoxesOverlay : Bool
    }


render :
    Texture
    -> RenderOptions
    -> { a | width : Int, height : Int }
    -> Vec2
    -> Float
    -> Maybe Options
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
render defaultTexture renderOptions viewport drag theta options (Scene scene) renderer =
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
        scene.rendererOptions
        renderOptions
        { sceneCamera = scene.camera
        , scenePerspective = options_.perspective aspectRatio
        , sceneMatrix = Mat4.identity
        , sceneRotationMatrix = Mat4.identity
        }
        defaultTexture
        scene.graph
        renderer


renderGraph :
    Vec2
    -> Float
    -> Renderer.Options
    -> RenderOptions
    -> Uniforms u
    -> Texture
    -> List (Graph materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta rendererOptions renderOptions uniforms defaultTexture graph renderer =
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

                            sceneRotationMatrix =
                                Object.rotation object_
                                    |> Mat4.mul uniforms.sceneRotationMatrix

                            entity : Uniforms u -> Entity
                            entity uniforms_ =
                                object
                                    |> Object.materialName
                                    |> renderer
                                    |> (\r -> r rendererOptions defaultTexture uniforms_ object)

                            boundingBox : Uniforms u -> Entity
                            boundingBox uniforms_ =
                                let
                                    vertices =
                                        object
                                            |> Object.boundingBox
                                            |> XYZMika.XYZ.Mesh.Cube.pairsColorfulWithBounds

                                    options =
                                        if renderOptions.showBoundingBoxesOverlay then
                                            [ WebGL.Settings.DepthTest.always
                                                { write = True
                                                , near = 0
                                                , far = 1
                                                }
                                            ]

                                        else
                                            [ WebGL.Settings.DepthTest.default ]
                                in
                                WebGL.entityWith
                                    options
                                    XYZMika.XYZ.Material.Simple.vertexShader
                                    XYZMika.XYZ.Material.Simple.fragmentShader
                                    (WebGL.lines vertices)
                                    uniforms_
                        in
                        case ( renderOptions.showGeometry, renderOptions.showBoundingBoxes ) of
                            ( True, True ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag theta rendererOptions renderOptions { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix } defaultTexture children renderer

                            ( True, False ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag theta rendererOptions renderOptions { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix } defaultTexture children renderer

                            ( False, True ) ->
                                boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag theta rendererOptions renderOptions { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix } defaultTexture children renderer

                            _ ->
                                []
            )
        |> List.concat
