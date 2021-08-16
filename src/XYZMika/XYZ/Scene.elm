module XYZMika.XYZ.Scene exposing
    ( GraphRenderOptions
    , Options
    , RenderOptions
    , Scene
    , camera
    , defaultOptions
    , defaultRenderOptions
    , direction
    , getGraph
    , graphWithMatrix
    , inDirection
    , init
    , map
    , pointLights
    , render
    , withCamera
    , withCameraMap
    , withCameraPosition
    , withPointLight1Map
    , withPointLight2Map
    , withRendererOptionsMap
    )

import Color
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Tree exposing (Tree)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.Color
import XYZMika.XYZ.Material as Renderer exposing (Renderer)
import XYZMika.XYZ.Material.Gizmo
import XYZMika.XYZ.Material.Grid
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Primitives
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Light as Light exposing (PointLight)
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
        { graph : Tree (Object materialId)
        , camera : Camera
        , rendererOptions : Renderer.Options
        , gizmoMaterial : materialId
        }


init : { gizmoMaterial : materialId } -> Tree (Object materialId) -> Scene materialId
init { gizmoMaterial } graph =
    Scene
        { graph = graph
        , camera = Camera.init (vec3 0 3 4) (vec3 0 0 0)
        , rendererOptions = Renderer.defaultOptions
        , gizmoMaterial = gizmoMaterial
        }


getGraph : Scene materialId -> Tree (Object materialId)
getGraph (Scene scene) =
    scene.graph


camera : Scene materialId -> Camera
camera (Scene scene) =
    scene.camera


pointLights : Scene materialId -> List PointLight
pointLights (Scene scene) =
    [ scene.rendererOptions.lights.point1
    , scene.rendererOptions.lights.point2
    ]


withPointLight1Map : (PointLight -> PointLight) -> Scene materialId -> Scene materialId
withPointLight1Map f (Scene scene) =
    let
        rendererOptions =
            scene.rendererOptions

        lights =
            rendererOptions.lights

        pointLight =
            lights.point1
    in
    Scene { scene | rendererOptions = { rendererOptions | lights = { lights | point1 = f pointLight } } }


withPointLight2Map : (PointLight -> PointLight) -> Scene materialId -> Scene materialId
withPointLight2Map f (Scene scene) =
    let
        rendererOptions =
            scene.rendererOptions

        lights =
            rendererOptions.lights

        pointLight =
            lights.point2
    in
    Scene { scene | rendererOptions = { rendererOptions | lights = { lights | point2 = f pointLight } } }


withRendererOptionsMap : (Renderer.Options -> Renderer.Options) -> Scene materialId -> Scene materialId
withRendererOptionsMap f (Scene scene) =
    Scene { scene | rendererOptions = f scene.rendererOptions }


withCamera : { position : Vec3, target : Vec3 } -> Scene materialId -> Scene materialId
withCamera { position, target } (Scene scene) =
    Scene { scene | camera = Camera.init position target }


withCameraPosition : Vec3 -> Scene materialId -> Scene materialId
withCameraPosition position (Scene scene) =
    Scene { scene | camera = scene.camera |> Camera.withPosition position }


withCameraMap : (Camera -> Camera) -> Scene materialId -> Scene materialId
withCameraMap f (Scene scene) =
    Scene { scene | camera = f scene.camera }


map : (Tree (Object materialId) -> Tree (Object materialId)) -> Scene materialId -> Scene materialId
map f (Scene scene) =
    Scene { scene | graph = scene.graph |> f }



--map : (Graph materialId -> Graph materialId) -> Scene materialId -> Scene materialId
--map f (Scene scene) =
--    Scene { scene | graph = scene.graph |> List.map f }


graphWithMatrix : { theta : Float, drag : Vec2, mat : Mat4 } -> Tree (Object materialId) -> Tree ( Mat4, Object materialId )
graphWithMatrix ({ theta, drag, mat } as config) tree =
    let
        object =
            Tree.label tree

        children : List (Tree (Object materialId))
        children =
            Tree.children tree

        mat_ =
            object
                |> Object.rotationWithDrag drag
                |> Object.rotationInTime theta
                |> Object.rotation
                |> Mat4.mul (Mat4.makeTranslate (Object.position object))
                |> Mat4.mul mat
    in
    Tree.singleton ( mat_, object ) |> Tree.replaceChildren (children |> List.map (graphWithMatrix { config | mat = mat_ }))


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


type alias GraphRenderOptions =
    { showBoundingBox : Bool
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
    -> (Tree ( Int, Object materialId ) -> Maybe GraphRenderOptions)
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
render defaultTexture renderOptions viewport drag theta options graphRenderOptions (Scene scene) renderer =
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
        graphRenderOptions
        { sceneCamera = Camera.toMat4 scene.camera
        , scenePerspective = options_.perspective aspectRatio
        , sceneMatrix = Mat4.identity
        , sceneRotationMatrix = Mat4.identity
        }
        defaultTexture
        (PointLightNode (Light.position scene.rendererOptions.lights.point1)
            :: PointLightNode (Light.position scene.rendererOptions.lights.point2)
            :: GraphNode (graphWithMatrix { theta = theta, drag = drag, mat = Mat4.identity } scene.graph |> Tree.indexedMap (\i ( sceneMatrix, object ) -> Renderable i sceneMatrix object))
            :: []
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


type Axis
    = AxisX
    | AxisY
    | AxisZ


type alias Renderable materialId =
    { index : Int, sceneMatrix : Mat4, object : Object materialId }


type Node materialId
    = GraphNode (Tree (Renderable materialId))
    | GridPlaneNode Axis
    | PointLightNode Vec3


renderGraph :
    Vec2
    -> Float
    -> Renderer.Options
    -> RenderOptions
    -> (Tree ( Int, Object materialId ) -> Maybe GraphRenderOptions)
    -> Uniforms u
    -> Texture
    -> List (Node materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta rendererOptions renderOptions graphRenderOptionsFn uniforms defaultTexture nodes renderer =
    nodes
        |> List.map
            (\node ->
                case node of
                    PointLightNode position ->
                        let
                            size =
                                1

                            mesh =
                                XYZMika.XYZ.Mesh.Primitives.quad Color.green
                                    (vec3 -size -size 0)
                                    (vec3 -size size 0)
                                    (vec3 size size 0)
                                    (vec3 size -size 0)
                                    |> WebGL.triangles
                        in
                        [ WebGL.entityWith
                            [ WebGL.Settings.DepthTest.default
                            , WebGL.Settings.sampleAlphaToCoverage
                            ]
                            XYZMika.XYZ.Material.Gizmo.vertexShader
                            XYZMika.XYZ.Material.Gizmo.fragmentShader
                            mesh
                            { sceneCamera = uniforms.sceneCamera
                            , scenePerspective = uniforms.scenePerspective
                            , sceneMatrix = uniforms.sceneMatrix
                            , sceneRotationMatrix = uniforms.sceneRotationMatrix

                            --
                            , center = position
                            }
                        ]

                    GridPlaneNode axis ->
                        let
                            size =
                                20

                            ( color, axisIndex, mesh ) =
                                case axis of
                                    AxisX ->
                                        ( Color.red
                                        , 0
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.green
                                            (vec3 0 -size -size)
                                            (vec3 0 -size size)
                                            (vec3 0 size size)
                                            (vec3 0 size -size)
                                            |> WebGL.triangles
                                        )

                                    AxisY ->
                                        ( Color.green
                                        , 1
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.yellow
                                            (vec3 -size 0 -size)
                                            (vec3 -size 0 size)
                                            (vec3 size 0 size)
                                            (vec3 size 0 -size)
                                            |> WebGL.triangles
                                        )

                                    AxisZ ->
                                        ( Color.blue
                                        , 2
                                        , XYZMika.XYZ.Mesh.Primitives.quad Color.yellow
                                            (vec3 -size -size 0)
                                            (vec3 -size size 0)
                                            (vec3 size size 0)
                                            (vec3 size -size 0)
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

                    GraphNode graph ->
                        let
                            { sceneMatrix, object } =
                                Tree.label graph

                            children : List (Tree (Renderable materialId))
                            children =
                                Tree.children graph

                            graphRenderOptions : Maybe GraphRenderOptions
                            graphRenderOptions =
                                graph
                                    |> Tree.map (\x -> ( x.index, x.object ))
                                    |> graphRenderOptionsFn

                            sceneRotationMatrix =
                                Object.rotation object
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

                                    overlay =
                                        graphRenderOptions
                                            |> Maybe.map .showBoundingBox
                                            |> Maybe.withDefault renderOptions.showBoundingBoxesOverlay

                                    options =
                                        if overlay then
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

                            showBoundingBox =
                                graphRenderOptions
                                    |> Maybe.map .showBoundingBox
                                    |> Maybe.withDefault renderOptions.showBoundingBoxes
                        in
                        case ( renderOptions.showGeometry, showBoundingBox ) of
                            ( True, True ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        renderOptions
                                        graphRenderOptionsFn
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
                                        graphRenderOptionsFn
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
                                        graphRenderOptionsFn
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        defaultTexture
                                        (List.map GraphNode children)
                                        renderer

                            _ ->
                                []
            )
        |> List.concat
