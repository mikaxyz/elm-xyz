module XYZMika.XYZ.Scene exposing
    ( Options
    , RenderOptions
    , Scene
    , camera
    , defaultRenderOptions
    , direction
    , inDirection
    , init
    , map
    , render
    , withCamera
    , withCameraMap
    , withCameraPosition
    , withRendererOptions
    )

import Color
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.Color
import XYZMika.XYZ.Material as Renderer exposing (Renderer)
import XYZMika.XYZ.Material.Grid
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Gizmo as Gizmo
import XYZMika.XYZ.Mesh.Primitives
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
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
        , camera : Camera
        , rendererOptions : Renderer.Options
        , gizmoMaterial : materialId
        }


withRendererOptions : Renderer.Options -> Scene materialId -> Scene materialId
withRendererOptions rendererOptions (Scene scene) =
    Scene { scene | rendererOptions = rendererOptions }


init : { gizmoMaterial : materialId } -> List (Graph materialId) -> Scene materialId
init { gizmoMaterial } graph =
    Scene
        { graph = graph
        , camera = Camera.init (vec3 0 3 4) (vec3 0 0 0)
        , rendererOptions = Renderer.defaultOptions
        , gizmoMaterial = gizmoMaterial
        }


camera : Scene materialId -> Camera
camera (Scene scene) =
    scene.camera


withCamera : { position : Vec3, target : Vec3 } -> Scene materialId -> Scene materialId
withCamera { position, target } (Scene scene) =
    Scene { scene | camera = Camera.init position target }


withCameraPosition : Vec3 -> Scene materialId -> Scene materialId
withCameraPosition position (Scene scene) =
    Scene { scene | camera = scene.camera |> Camera.withPosition position }


withCameraMap : (Camera -> Camera) -> Scene materialId -> Scene materialId
withCameraMap f (Scene scene) =
    Scene { scene | camera = f scene.camera }


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
    , showGridX : Bool
    , showGridY : Bool
    , showGridZ : Bool
    }


defaultRenderOptions : RenderOptions
defaultRenderOptions =
    { showGeometry = True
    , showBoundingBoxes = False
    , showBoundingBoxesOverlay = False
    , showGridX = False
    , showGridY = True
    , showGridZ = False
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
        { sceneCamera = Camera.toMat4 scene.camera
        , scenePerspective = options_.perspective aspectRatio
        , sceneMatrix = Mat4.identity
        , sceneRotationMatrix = Mat4.identity
        }
        defaultTexture
        (([ scene.rendererOptions.lights.point1.position
          , scene.rendererOptions.lights.point2.position
          ]
            |> addGizmos scene.gizmoMaterial scene.graph
            |> List.map GraphNode
         )
            |> withGridPlane renderOptions.showGridX AxisX
            |> withGridPlane renderOptions.showGridY AxisY
            |> withGridPlane renderOptions.showGridZ AxisZ
        )
        renderer


withGridPlane : Bool -> Axis -> List (Node materialId) -> List (Node materialId)
withGridPlane show axis nodes =
    if show then
        GridPlaneNode axis :: nodes

    else
        nodes


addGizmos : materialId -> List (Graph materialId) -> List Vec3 -> List (Graph materialId)
addGizmos material graph pointLights =
    graph
        ++ (pointLights
                |> List.map
                    (\position ->
                        Graph
                            (Gizmo.axis
                                |> Object.init
                                |> Object.withMaterialName material
                                |> Object.withPosition position
                                |> Object.withGlSetting
                                    (WebGL.Settings.DepthTest.always
                                        { write = True
                                        , near = 0
                                        , far = 1
                                        }
                                    )
                            )
                            []
                    )
           )


type Axis
    = AxisX
    | AxisY
    | AxisZ


type Node materialId
    = GraphNode (Graph materialId)
    | GridPlaneNode Axis


renderGraph :
    Vec2
    -> Float
    -> Renderer.Options
    -> RenderOptions
    -> Uniforms u
    -> Texture
    -> List (Node materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta rendererOptions renderOptions uniforms defaultTexture nodes renderer =
    nodes
        |> List.map
            (\node ->
                case node of
                    GridPlaneNode axis ->
                        let
                            gridSize =
                                20

                            ( color, axisIndex, mesh ) =
                                case axis of
                                    AxisX ->
                                        ( Color.red
                                        , 0
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.green
                                            (vec3 0 -gridSize -gridSize)
                                            (vec3 0 -gridSize gridSize)
                                            (vec3 0 gridSize gridSize)
                                            (vec3 0 gridSize -gridSize)
                                            |> WebGL.triangles
                                        )

                                    AxisY ->
                                        ( Color.green
                                        , 1
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.yellow
                                            (vec3 -gridSize 0 -gridSize)
                                            (vec3 -gridSize 0 gridSize)
                                            (vec3 gridSize 0 gridSize)
                                            (vec3 gridSize 0 -gridSize)
                                            |> WebGL.triangles
                                        )

                                    AxisZ ->
                                        ( Color.blue
                                        , 2
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.yellow
                                            (vec3 -gridSize -gridSize 0)
                                            (vec3 -gridSize gridSize 0)
                                            (vec3 gridSize gridSize 0)
                                            (vec3 gridSize -gridSize 0)
                                            |> WebGL.triangles
                                        )
                        in
                        [ WebGL.entityWith
                            [ WebGL.Settings.DepthTest.default
                            , WebGL.Settings.sampleAlphaToCoverage
                            ]
                            XYZMika.XYZ.Material.Grid.vertexShader
                            XYZMika.XYZ.Material.Grid.fragmentShader
                            mesh
                            { sceneCamera = uniforms.sceneCamera
                            , scenePerspective = uniforms.scenePerspective
                            , sceneMatrix = uniforms.sceneMatrix
                            , sceneRotationMatrix = uniforms.sceneRotationMatrix

                            --
                            , color = XYZMika.Color.toVec3 color
                            , axis = axisIndex
                            }
                        ]

                    GraphNode (Graph object children) ->
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
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        renderOptions
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        defaultTexture
                                        (List.map GraphNode children)
                                        renderer

                            ( True, False ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        renderOptions
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        defaultTexture
                                        (List.map GraphNode children)
                                        renderer

                            ( False, True ) ->
                                boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        renderOptions
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        defaultTexture
                                        (List.map GraphNode children)
                                        renderer

                            _ ->
                                []
            )
        |> List.concat
