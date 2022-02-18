module Update exposing (update)

import Browser.Dom
import Color
import File.Download
import Keyboard
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Model exposing (Hud(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import Task
import XYZMika.Debug as Dbug
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Parser.Serialize
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Graph as Graph
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object
import XYZMika.XYZ.Scene.Options as SceneOptions
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
                updateObject : Object.Object objectId materialId -> Object.Object objectId materialId
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
                                (Graph.indexedMap
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
                    Dbug.log "error" error
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
            ( if model.paused then
                model

              else
                { model | theta = model.theta + (elapsed / 10000) }
            , Cmd.none
            )

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
                selectedTreeIndexAtClickPosition : Scene.Scene objectId materialId -> Browser.Dom.Element -> Maybe Int
                selectedTreeIndexAtClickPosition scene viewPortElement =
                    Util.selectGraphAtClickPosition
                        { theta = model.theta
                        , drag = model.drag
                        , viewport = Model.viewport
                        , viewPortElement = viewPortElement
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
                Keyboard.Alpha 'P' ->
                    ( { model | paused = not model.paused }
                    , Cmd.none
                    )

                Keyboard.Alpha 'X' ->
                    ( { model
                        | scene =
                            model.scene
                                |> Maybe.map
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 0 -lightDistance 0)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 0 lightDistance 0)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 lightDistance 0 0)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 -lightDistance 0 0)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 0 0 -lightDistance)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 0 0 lightDistance)
                                            |> Light.withColor Color.white
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 -4 2 4)
                                            |> Light.withColor Color.red
                                            |> Light.withIntensity 0.5
                                        , Light.pointLight (vec3 4 2 4)
                                            |> Light.withColor Color.green
                                            |> Light.withIntensity 0.5
                                        , Light.pointLight (vec3 0 2 -4)
                                            |> Light.withColor Color.blue
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
                                    (Scene.withLights
                                        [ Light.pointLight (vec3 -4 2 2)
                                            |> Light.withColor Color.white
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 -2 4 0.5)
                                            |> Light.withColor Color.white
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 0 5 0)
                                            |> Light.withColor Color.white
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 2 4 0.5)
                                            |> Light.withColor Color.white
                                            |> Light.withIntensity 0.2
                                        , Light.pointLight (vec3 4 2 2)
                                            |> Light.withColor Color.white
                                            |> Light.withIntensity 0.2
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'S' ->
                    ( { model | scene = model.scene |> Maybe.map Scene.withLightsInGraph }
                    , Cmd.none
                    )

                Keyboard.Digit 1 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showGeometryOption)
                    , Cmd.none
                    )

                Keyboard.Digit 2 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showBoundingBoxesOption)
                    , Cmd.none
                    )

                Keyboard.Digit 3 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showBoundingBoxesOverlayOption)
                    , Cmd.none
                    )

                Keyboard.Digit 4 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showLightGizmosOption)
                    , Cmd.none
                    )

                Keyboard.Digit 7 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showGridXOption)
                    , Cmd.none
                    )

                Keyboard.Digit 8 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showGridYOption)
                    , Cmd.none
                    )

                Keyboard.Digit 9 ->
                    ( model |> Model.mapSceneOptions (SceneOptions.toggle SceneOptions.showGridZOption)
                    , Cmd.none
                    )

                Keyboard.Alpha 'J' ->
                    model |> Model.nextScene

                Keyboard.Alpha 'K' ->
                    model |> Model.prevScene

                _ ->
                    ( model, Cmd.none )

        AssetStoreLoadResult result ->
            case result of
                Ok asset ->
                    onResize
                        ( model
                            |> Model.updateAssetStore (AssetStore.addToStore asset model.assets)
                        , Cmd.none
                        )

                Err error ->
                    let
                        _ =
                            Dbug.log "error" error
                    in
                    ( model
                    , Cmd.none
                    )

        AssetStoreLoadResultDownloadXyz name assetId asset ->
            let
                modelUpdated =
                    Model.updateAssetStore (AssetStore.addToStore asset model.assets) model

                download =
                    Maybe.map2
                        (\a b ->
                            { triangles = a
                            , indexedTriangles = b
                            }
                                |> XYZMika.XYZ.Parser.Serialize.toJsonString
                                |> File.Download.string (name ++ ".xyz") "text/json"
                        )
                        (AssetStore.vertices assetId modelUpdated.assets)
                        (AssetStore.verticesIndexed assetId modelUpdated.assets)
                        |> Maybe.withDefault Cmd.none
            in
            onResize
                ( modelUpdated
                , download
                )
