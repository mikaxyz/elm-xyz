module Update exposing (update)

import Browser.Dom
import Keyboard
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Model exposing (Hud(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import Scenes.ObjectLoader
import Task
import Tree exposing (Tree)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Parser.Obj
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object
import XYZMika.XYZ.Scene.Util as Util


onResize ( model, cmd ) =
    ( model
    , Cmd.batch [ cmd, Browser.Dom.getElement "viewport" |> Task.attempt OnViewportElement ]
    )


updateHud : HudMsg -> Hud -> ( Hud, Cmd Msg )
updateHud msg (Hud hud) =
    case msg of
        Click ->
            ( Hud hud, Cmd.none )

        ToggleSidebar ->
            onResize ( Hud { hud | sidebarExpanded = not hud.sidebarExpanded }, Cmd.none )


applyHudValue : HudObject -> HudValue -> Float -> Model -> Model
applyHudValue hudObject hudValue value model =
    case hudObject of
        Camera ->
            { model
                | scene =
                    model.scene
                        |> Maybe.map
                            (Scene.withCameraMap
                                (\camera ->
                                    case hudValue of
                                        HudValue_Vec3_X ->
                                            Camera.withPositionMap (Vec3.setX value) camera

                                        HudValue_Vec3_Y ->
                                            Camera.withPositionMap (Vec3.setY value) camera

                                        HudValue_Vec3_Z ->
                                            Camera.withPositionMap (Vec3.setZ value) camera

                                        HudValue_Vec3_Roll ->
                                            Camera.withOrbitY (value - Camera.roll camera) camera
                                )
                            )
            }

        SelectedGraph ->
            let
                updateObject : Object.Object materialId -> Object.Object materialId
                updateObject object =
                    let
                        position =
                            case hudValue of
                                HudValue_Vec3_X ->
                                    object
                                        |> Object.position
                                        |> Vec3.setX value

                                HudValue_Vec3_Y ->
                                    object
                                        |> Object.position
                                        |> Vec3.setY value

                                HudValue_Vec3_Z ->
                                    object
                                        |> Object.position
                                        |> Vec3.setZ value

                                HudValue_Vec3_Roll ->
                                    object
                                        |> Object.position
                    in
                    Object.withPosition position object
            in
            { model
                | scene =
                    model.scene
                        |> Maybe.map
                            (Scene.map
                                (Tree.indexedMap
                                    (\index object ->
                                        if model.selectedTreeIndex == Just index then
                                            updateObject object

                                        else
                                            object
                                    )
                                )
                            )
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResize ->
            onResize ( model, Cmd.none )

        OnViewportElement (Ok x) ->
            ( { model | viewPortElement = Just x }, Cmd.none )

        OnViewportElement (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( model, Cmd.none )

        HudMsg msg_ ->
            updateHud msg_ model.hud
                |> Tuple.mapFirst (\hud -> { model | hud = hud })

        SetValue hudObject hudValue x ->
            ( case String.toFloat x of
                Just value ->
                    applyHudValue hudObject hudValue value model

                Nothing ->
                    model
            , Cmd.none
            )

        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }, Cmd.none )

        DragStart target pos ->
            ( { model
                | dragger = Just { from = pos, to = pos }
                , lastDrag = pos
                , dragTarget = target
              }
            , Cmd.none
            )

        DragBy d ->
            ( { model
                | lastDrag = Vec2.add model.lastDrag d
                , scene =
                    case model.dragTarget of
                        Model.CameraOrbit ->
                            model.scene
                                |> Maybe.map
                                    (Scene.withCameraMap
                                        (\camera ->
                                            camera
                                                |> Camera.withOrbitY -(Vec2.getX d / 100)
                                                --|> Camera.orbitX -(Vec2.getY d / 100)
                                                |> Camera.withPositionMap (\position -> Vec3.setY (Vec3.getY position + (Vec2.getY d / 20)) position)
                                        )
                                    )

                        Model.CameraPan ->
                            model.scene |> Maybe.map (Scene.withCameraMap (Camera.withPan (Vec2.scale 0.01 d)))

                        Model.CameraZoom ->
                            model.scene |> Maybe.map (Scene.withCameraMap (Camera.withZoom (Vec2.getY d / 20)))

                        Model.Default ->
                            model.scene
              }
            , Cmd.none
            )

        Drag pos ->
            if model.dragTarget == Model.Default then
                ( { model
                    | dragger = Maybe.map (\drag -> { drag | to = pos }) model.dragger
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        DragEnd pos ->
            let
                selectedTreeIndexAtClickPosition : Scene.Scene materialId -> Browser.Dom.Element -> Maybe Int
                selectedTreeIndexAtClickPosition scene viewPortElement =
                    Util.selectGraphAtClickPosition
                        { theta = model.theta
                        , drag = model.drag
                        , viewport = Model.viewport
                        , viewPortElement = viewPortElement
                        , sceneOptions = Model.sceneOptions model
                        }
                        scene
                        ( Vec2.getX pos, Vec2.getY pos )
                        |> Maybe.map Tuple.first
            in
            ( { model
                | dragger = Nothing
                , dragTarget = Model.Default
                , drag =
                    model.dragger
                        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
                        |> Maybe.withDefault model.drag
                , selectedTreeIndex =
                    if model.dragTarget == Model.Default then
                        Maybe.map2 Tuple.pair model.scene model.viewPortElement
                            |> Maybe.andThen
                                (\( scene, viewPortElement ) ->
                                    selectedTreeIndexAtClickPosition scene viewPortElement
                                )

                    else
                        model.selectedTreeIndex
              }
            , Cmd.none
            )

        KeyboardMsg msg_ ->
            ( { model | keyboard = Keyboard.update msg_ model.keyboard }
            , Cmd.none
            )

        OnKeyDown key ->
            let
                lightDistance =
                    6
            in
            case key of
                Keyboard.Alpha 'X' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 0 -lightDistance 0)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'W' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 0 lightDistance 0)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'D' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 lightDistance 0 0)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'A' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 -lightDistance 0 0)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'E' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 0 0 -lightDistance)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'Z' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 0 0 lightDistance)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'C' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 -4 2 4)
                                            |> Light.withColor (vec3 1 0 0)
                                            |> Light.withIntensity 0.5
                                        , Light.pointLight (vec3 4 2 4)
                                            |> Light.withColor (vec3 0 1 0)
                                            |> Light.withIntensity 0.5
                                        , Light.pointLight (vec3 0 2 -4)
                                            |> Light.withColor (vec3 0 0 1)
                                            |> Light.withIntensity 0.5
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'Q' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.replaceLightsWithLightsInRoot
                                        [ Light.pointLight (vec3 -4 2 2)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 -2 4 0.5)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 0 5 0)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 2 4 0.5)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 4 2 2)
                                            |> Light.withColor (vec3 1 1 1)
                                            |> Light.withIntensity 0.2
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'S' ->
                    ( model |> Model.mapRendererOptions (always XYZMika.XYZ.Material.defaultOptions)
                    , Cmd.none
                    )

                Keyboard.Digit 1 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showGeometry = not x.showGeometry })
                    , Cmd.none
                    )

                Keyboard.Digit 2 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showBoundingBoxes = not x.showBoundingBoxes })
                    , Cmd.none
                    )

                Keyboard.Digit 3 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showBoundingBoxesOverlay = not x.showBoundingBoxesOverlay })
                    , Cmd.none
                    )

                Keyboard.Digit 4 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showGridX = not x.showGridX })
                    , Cmd.none
                    )

                Keyboard.Digit 5 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showGridY = not x.showGridY })
                    , Cmd.none
                    )

                Keyboard.Digit 6 ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showGridZ = not x.showGridZ })
                    , Cmd.none
                    )

                Keyboard.Alpha 'J' ->
                    model |> Model.nextScene

                Keyboard.Alpha 'K' ->
                    model |> Model.prevScene

                _ ->
                    ( model, Cmd.none )

        GotObj material ( options, pos, str ) ->
            ( { model
                | scene =
                    model.scene
                        |> Maybe.map
                            (\scene ->
                                Scenes.ObjectLoader.addMesh
                                    material
                                    (XYZMika.XYZ.Parser.Obj.parse options str).triangles
                                    pos
                                    scene
                            )
              }
            , Cmd.none
            )

        AssetLoaded scale asset ->
            onResize
                ( model
                    |> Model.updateAssetStore (AssetStore.addToStore scale asset model.assets)
                , Cmd.none
                )
