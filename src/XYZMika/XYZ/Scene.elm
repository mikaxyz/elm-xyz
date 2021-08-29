module XYZMika.XYZ.Scene exposing
    ( GraphRenderOptions
    , Options
    , Scene
    , camera
    , defaultOptions
    , getGraph
    , graphWithMatrix
    , init
    , map
    , render
    , replaceLightsWithLightsInRoot
    , withCamera
    , withCameraMap
    , withCameraPosition
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
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
import XYZMika.XYZ.Scene.Options as SceneOptions
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Scene materialId
    = Scene
        { graph : Tree (Object materialId)
        , camera : Camera
        }


init : Tree (Object materialId) -> Scene materialId
init graph =
    Scene
        { graph = graph
        , camera = Camera.init (vec3 0 3 4) (vec3 0 0 0)
        }


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


withCameraMap : (Camera -> Camera) -> Scene materialId -> Scene materialId
withCameraMap f (Scene scene) =
    Scene { scene | camera = f scene.camera }


map : (Tree (Object materialId) -> Tree (Object materialId)) -> Scene materialId -> Scene materialId
map f (Scene scene) =
    Scene { scene | graph = scene.graph |> f }


replaceLightsWithLightsInRoot : List Light.Light -> Scene materialId -> Scene materialId
replaceLightsWithLightsInRoot lights scene =
    scene
        |> map
            (\tree ->
                Tree.tree
                    (Object.initWithTriangles [])
                    ((tree
                        |> Tree.map
                            (\object ->
                                case Object.maybeLight object of
                                    Just _ ->
                                        Object.toEmpty object

                                    Nothing ->
                                        object
                            )
                     )
                        :: (lights
                                |> List.map
                                    (\light ->
                                        Object.light
                                            (Light.position light
                                                |> Maybe.withDefault (vec3 0 0 0)
                                            )
                                            light
                                            |> Tree.singleton
                                    )
                           )
                    )
            )


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


type alias GraphRenderOptions =
    { showBoundingBox : Bool
    }


render :
    Texture
    -> List Light
    -> SceneOptions.Options
    -> { a | width : Int, height : Int }
    -> Vec2
    -> Float
    -> Maybe Options
    -> (Tree ( Int, Object materialId ) -> Maybe GraphRenderOptions)
    -> Scene materialId
    -> Renderer materialId (Uniforms {})
    -> List Entity
render defaultTexture defaultLights sceneOptions viewport drag theta options graphRenderOptions (Scene scene) renderer =
    --TODO: Remove defaultTexture. Require a texture in object if Advanced renderer?
    let
        options_ =
            options |> Maybe.withDefault defaultOptions

        aspectRatio =
            toFloat viewport.width / toFloat viewport.height

        lightsInGraph : List ( Mat4, Light )
        lightsInGraph =
            graphWithMatrix { theta = theta, drag = drag, mat = Mat4.identity } scene.graph
                |> Tree.indexedMap (\i ( sceneMatrix, object ) -> Renderable i sceneMatrix object)
                |> Tree.foldl
                    (\{ index, sceneMatrix, object } acc ->
                        Object.maybeLight object
                            |> Maybe.map (\light -> ( sceneMatrix, light ) :: acc)
                            |> Maybe.withDefault acc
                    )
                    []

        ( numberOfLights, rendererOptionsWithLights ) =
            lightsInGraph
                |> List.foldl
                    (\( transform, light ) ( index, acc ) ->
                        ( index + 1
                        , acc |> Renderer.addLight (Light.withPosition (vec3 0 0 0 |> Mat4.transform transform) light)
                        )
                    )
                    ( 0, Renderer.createOptions |> Renderer.withLights [] )

        rendererOptions : Renderer.Options
        rendererOptions =
            if numberOfLights > 0 then
                rendererOptionsWithLights

            else
                Renderer.createOptions |> Renderer.withLights defaultLights
    in
    renderGraph
        drag
        theta
        rendererOptions
        sceneOptions
        graphRenderOptions
        { sceneCamera = Camera.toMat4 scene.camera
        , scenePerspective = options_.perspective aspectRatio
        , sceneMatrix = Mat4.identity
        , sceneRotationMatrix = Mat4.identity
        }
        defaultTexture
        (GraphNode (graphWithMatrix { theta = theta, drag = drag, mat = Mat4.identity } scene.graph |> Tree.indexedMap (\i ( sceneMatrix, object ) -> Renderable i sceneMatrix object))
            :: (Renderer.lights rendererOptions
                    |> List.filterMap Light.position
                    |> List.map PointLightNode
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
    -> Texture
    -> List (Node materialId)
    -> Renderer materialId (Uniforms u)
    -> List Entity
renderGraph drag theta rendererOptions sceneOptions graphRenderOptionsFn uniforms defaultTexture nodes renderer =
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
                        in
                        case ( showGeometry, showBoundingBox ) of
                            ( True, True ) ->
                                entity { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: boundingBox { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    :: renderGraph drag
                                        theta
                                        rendererOptions
                                        sceneOptions
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
                                        sceneOptions
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
                                        sceneOptions
                                        graphRenderOptionsFn
                                        { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                        defaultTexture
                                        (List.map GraphNode children)
                                        renderer

                            _ ->
                                renderGraph drag
                                    theta
                                    rendererOptions
                                    sceneOptions
                                    graphRenderOptionsFn
                                    { uniforms | sceneMatrix = sceneMatrix, sceneRotationMatrix = sceneRotationMatrix }
                                    defaultTexture
                                    (List.map GraphNode children)
                                    renderer
            )
        |> List.concat
