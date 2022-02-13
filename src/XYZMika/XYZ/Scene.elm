module XYZMika.XYZ.Scene exposing
    ( GraphRenderOptions
    , Modifier(..)
    , Scene
    , camera
    , getGraph
    , graphWithMatrix
    , init
    , map
    , projectionMatrix
    , render
    , renderSimple
    , renderSimpleWithModifiers
    , spotLights
    , withCamera
    , withCameraMap
    , withCameraPosition
    , withCameraTarget
    , withLights
    , withLightsInGraph
    , withModifiers
    , withPerspectiveProjection
    )

import Color
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Tree exposing (Tree)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest
import XYZMika.Color
import XYZMika.XYZ.Material as Renderer exposing (Renderer)
import XYZMika.XYZ.Material.Gizmo
import XYZMika.XYZ.Material.Grid
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Mesh.Primitives
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.SpotLight exposing (SpotLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Options as SceneOptions
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Scene materialId
    = Scene
        { graph : Tree (Object materialId)
        , camera : Camera
        , projection : Projection
        }


type Projection
    = Perspective { fov : Float, near : Float, far : Float }


init : Tree (Object materialId) -> Scene materialId
init graph =
    Scene
        { graph = graph
        , camera = Camera.init (vec3 0 3 4) (vec3 0 0 0)
        , projection = Perspective { fov = 45, far = 100.0, near = 0.01 }
        }


projectionMatrix : Float -> Scene materialId -> Mat4
projectionMatrix aspectRatio (Scene scene) =
    case scene.projection of
        Perspective { fov, near, far } ->
            Mat4.makePerspective fov aspectRatio near far


withPerspectiveProjection : { fov : Float, near : Float, far : Float } -> Scene materialId -> Scene materialId
withPerspectiveProjection config (Scene scene) =
    Scene { scene | projection = Perspective config }


getGraph : Scene materialId -> Tree (Object materialId)
getGraph (Scene scene) =
    scene.graph


camera : Scene materialId -> Camera
camera (Scene scene) =
    scene.camera


withCamera : { position : Vec3, target : Vec3 } -> Scene materialId -> Scene materialId
withCamera { position, target } (Scene scene) =
    Scene { scene | camera = Camera.init position target }


withCameraPosition : Vec3 -> Scene materialId -> Scene materialId
withCameraPosition position (Scene scene) =
    Scene { scene | camera = scene.camera |> Camera.withPosition position }


withCameraTarget : Vec3 -> Scene materialId -> Scene materialId
withCameraTarget target (Scene scene) =
    Scene { scene | camera = scene.camera |> Camera.withTarget target }


withCameraMap : (Camera -> Camera) -> Scene materialId -> Scene materialId
withCameraMap f (Scene scene) =
    Scene { scene | camera = f scene.camera }


map : (Tree (Object materialId) -> Tree (Object materialId)) -> Scene materialId -> Scene materialId
map f (Scene scene) =
    Scene { scene | graph = scene.graph |> f }


spotLights : Scene materialId -> List SpotLight
spotLights (Scene scene) =
    scene.graph
        |> Tree.foldl
            (\obj acc ->
                case Object.maybeLight obj |> Maybe.andThen Light.maybeSpotLight of
                    Just light ->
                        light :: acc

                    Nothing ->
                        acc
            )
            []


withLightsInGraph : Scene materialId -> Scene materialId
withLightsInGraph scene =
    scene
        |> setSceneLightsEnabled True
        |> replaceLightsInRoot []


withLights : List Light.Light -> Scene materialId -> Scene materialId
withLights lights scene =
    scene
        |> setSceneLightsEnabled False
        |> replaceLightsInRoot lights


setSceneLightsEnabled : Bool -> Scene materialId -> Scene materialId
setSceneLightsEnabled enabled scene =
    let
        enable object =
            case Object.maybeLightDisabled object of
                Just _ ->
                    Object.enable object

                Nothing ->
                    object

        disable object =
            case Object.maybeLight object of
                Just _ ->
                    Object.disable object

                Nothing ->
                    object
    in
    scene
        |> map
            (\tree ->
                tree
                    |> Tree.map
                        (if enabled then
                            enable

                         else
                            disable
                        )
            )


replaceLightsInRoot : List Light -> Scene materialId -> Scene materialId
replaceLightsInRoot lights scene =
    let
        appendLightsGroup tree =
            case lights of
                [] ->
                    tree

                nonEmptyList ->
                    nonEmptyList
                        |> List.map (Object.light >> Tree.singleton)
                        |> Tree.tree (Object.group "defaultLights")
                        |> (\x -> Tree.appendChild x tree)

        removeExisting tree =
            case Tree.label tree |> Object.maybeGroup "defaultLights" of
                Just _ ->
                    Nothing

                Nothing ->
                    Just tree
    in
    scene |> map (Tree.mapChildren (List.filterMap removeExisting) >> appendLightsGroup)


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


type alias GraphRenderOptions =
    { showBoundingBox : Bool
    }


renderSimple :
    { width : Int, height : Int }
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
renderSimple viewport scene renderer =
    render
        []
        []
        SceneOptions.create
        viewport
        (Math.Vector2.vec2 0 0)
        0.0
        (\_ -> Nothing)
        scene
        renderer


renderSimpleWithModifiers :
    List Modifier
    -> { width : Int, height : Int }
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
renderSimpleWithModifiers modifiers viewport scene renderer =
    render
        []
        modifiers
        SceneOptions.create
        viewport
        (Math.Vector2.vec2 0 0)
        0.0
        (\_ -> Nothing)
        scene
        renderer


type Modifier
    = PositionModifier (Int -> Vec3 -> Vec3)
    | RotationModifier (Int -> Mat4 -> Mat4)
    | SpotLightTargetModifier (Int -> Vec3 -> Vec3)


applyModifier : Int -> Object materialId -> Modifier -> Object materialId
applyModifier index object modifier =
    case modifier of
        PositionModifier f ->
            Object.withPosition (f index (Object.position object)) object

        RotationModifier f ->
            Object.withRotation (f index (Object.rotation object)) object

        SpotLightTargetModifier f ->
            case Object.maybeLight object of
                Just _ ->
                    Object.lightTargetMap (f index) object

                Nothing ->
                    object


applyModifiers : List Modifier -> Int -> Object materialId -> Object materialId
applyModifiers modifiers index object =
    modifiers
        |> List.foldl
            (\animation acc ->
                applyModifier index acc animation
            )
            object


withModifiers : List Modifier -> Scene materialId -> Scene materialId
withModifiers modifiers (Scene scene) =
    Scene
        { scene
            | graph =
                scene.graph
                    |> Tree.indexedMap
                        (\index object ->
                            applyModifiers modifiers index object
                        )
        }


render :
    List Light
    -> List Modifier
    -> SceneOptions.Options
    -> { width : Int, height : Int }
    -> Vec2
    -> Float
    -> (Tree ( Int, Object materialId ) -> Maybe GraphRenderOptions)
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
render defaultLights modifiers sceneOptions viewport drag theta graphRenderOptions (Scene scene) renderer =
    let
        aspectRatio =
            toFloat viewport.width / toFloat viewport.height

        lightsInScene : List Light
        lightsInScene =
            scene.graph
                |> Tree.indexedMap (\index object -> applyModifiers modifiers index object)
                |> graphWithMatrix { theta = theta, drag = drag, mat = Mat4.identity }
                |> Tree.foldl
                    (\( transform, object ) acc ->
                        case Object.maybeLight object of
                            Just light ->
                                Light.withPosition (vec3 0 0 0 |> Mat4.transform transform) light :: acc

                            Nothing ->
                                acc
                    )
                    []

        rendererOptions : Renderer.Options
        rendererOptions =
            case lightsInScene of
                [] ->
                    Renderer.createOptions |> Renderer.withLights defaultLights

                lights ->
                    Renderer.createOptions |> Renderer.withLights lights
    in
    renderGraph
        drag
        theta
        rendererOptions
        sceneOptions
        graphRenderOptions
        { sceneCamera = Camera.toMat4 scene.camera
        , scenePerspective = projectionMatrix aspectRatio (Scene scene)
        , sceneMatrix = Mat4.identity
        , sceneRotationMatrix = Mat4.identity
        }
        (GraphNode
            (scene.graph
                |> Tree.indexedMap (\index object -> applyModifiers modifiers index object)
                |> graphWithMatrix { theta = theta, drag = drag, mat = Mat4.identity }
                |> Tree.indexedMap
                    (\i ( sceneMatrix, object ) ->
                        Renderable i sceneMatrix object
                    )
            )
            :: (if SceneOptions.showLightGizmos sceneOptions then
                    Renderer.lights rendererOptions
                        |> List.filterMap Light.position
                        |> List.map PointLightNode

                else
                    []
               )
            |> withGridPlane (SceneOptions.showGridX sceneOptions) AxisX
            |> withGridPlane (SceneOptions.showGridY sceneOptions) AxisY
            |> withGridPlane (SceneOptions.showGridZ sceneOptions) AxisZ
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
    -> SceneOptions.Options
    -> (Tree ( Int, Object materialId ) -> Maybe GraphRenderOptions)
    -> Uniforms u
    -> List (Node materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta rendererOptions sceneOptions graphRenderOptionsFn uniforms nodes renderer =
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

                            sceneRotationMatrix : Mat4
                            sceneRotationMatrix =
                                object
                                    |> Object.rotationWithDrag drag
                                    |> Object.rotationInTime theta
                                    |> Object.rotation
                                    |> Mat4.mul uniforms.sceneRotationMatrix

                            entity : Uniforms u -> Entity
                            entity uniforms_ =
                                object
                                    |> Object.materialName
                                    |> renderer
                                    |> (\r -> r rendererOptions uniforms_ object)

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
                                            |> Maybe.withDefault (SceneOptions.showBoundingBoxesOverlay sceneOptions)

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
                                    |> Maybe.withDefault (SceneOptions.showBoundingBoxes sceneOptions)

                            showGeometry =
                                Object.maybeLight object
                                    |> Maybe.map (always False)
                                    |> Maybe.withDefault (SceneOptions.showGeometry sceneOptions)

                            enabled =
                                not (Object.isDisabled object)
                        in
                        case ( enabled && showGeometry, enabled && showBoundingBox ) of
                            ( True, True ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        sceneOptions
                                        graphRenderOptionsFn
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        (List.map GraphNode children)
                                        renderer

                            ( True, False ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        sceneOptions
                                        graphRenderOptionsFn
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        (List.map GraphNode children)
                                        renderer

                            ( False, True ) ->
                                boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        sceneOptions
                                        graphRenderOptionsFn
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        (List.map GraphNode children)
                                        renderer

                            _ ->
                                renderGraph drag
                                    theta
                                    rendererOptions
                                    sceneOptions
                                    graphRenderOptionsFn
                                    { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    (List.map GraphNode children)
                                    renderer
            )
        |> List.concat
