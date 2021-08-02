module Update exposing (update)

import Keyboard
import Math.Vector2 as Vec2
import Math.Vector3 as Vec3 exposing (vec3)
import Model exposing (Hud(..), HudLightObject(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import Scenes.ObjectLoader
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Parser.Obj
import XYZMika.XYZ.Scene
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Light as Light


pointLightDistance =
    5


updateHud : HudMsg -> Hud -> ( Hud, Cmd Msg )
updateHud msg (Hud hud) =
    case msg |> Debug.log "MSG" of
        Click ->
            ( Hud hud, Cmd.none )

        ToggleSidebar ->
            ( Hud { hud | sidebarExpanded = not hud.sidebarExpanded }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HudMsg msg_ ->
            updateHud msg_ model.hud
                |> Tuple.mapFirst (\hud -> { model | hud = hud })

        SetValue hudObject hudValue x ->
            ( case String.toFloat x of
                Just value ->
                    { model
                        | scene =
                            model.scene
                                --|> Maybe.map (XYZMika.XYZ.Scene.withCameraPosition (vec3 value 0 0))
                                |> Maybe.map
                                    (case hudObject of
                                        Camera ->
                                            XYZMika.XYZ.Scene.withCameraMap
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

                                        LightHudObject lightHudObject ->
                                            (case lightHudObject of
                                                PointLight1 ->
                                                    XYZMika.XYZ.Scene.withPointLight1Map

                                                PointLight2 ->
                                                    XYZMika.XYZ.Scene.withPointLight2Map
                                            )
                                                (\light ->
                                                    case hudValue of
                                                        HudValue_Vec3_X ->
                                                            Light.withPositionMap (Vec3.setX value) light

                                                        HudValue_Vec3_Y ->
                                                            Light.withPositionMap (Vec3.setY value) light

                                                        HudValue_Vec3_Z ->
                                                            Light.withPositionMap (Vec3.setZ value) light

                                                        HudValue_Vec3_Roll ->
                                                            light
                                                )
                                    )
                    }

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
                                    (XYZMika.XYZ.Scene.withCameraMap
                                        (\camera ->
                                            camera
                                                |> Camera.withOrbitY -(Vec2.getX d / 100)
                                                --|> Camera.orbitX -(Vec2.getY d / 100)
                                                |> Camera.withPositionMap (\position -> Vec3.setY (Vec3.getY position + (Vec2.getY d / 20)) position)
                                        )
                                    )

                        Model.CameraPan ->
                            model.scene |> Maybe.map (XYZMika.XYZ.Scene.withCameraMap (Camera.withPan (Vec2.scale 0.01 d)))

                        Model.CameraZoom ->
                            model.scene |> Maybe.map (XYZMika.XYZ.Scene.withCameraMap (Camera.withZoom (Vec2.getY d / 20)))

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
            ( { model
                | dragger = Nothing
                , dragTarget = Model.Default
                , drag =
                    model.dragger
                        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
                        |> Maybe.withDefault model.drag
              }
            , Cmd.none
            )

        KeyboardMsg msg_ ->
            ( { model | keyboard = Keyboard.update msg_ model.keyboard }
            , Cmd.none
            )

        OnKeyDown key ->
            case key of
                Keyboard.Alpha 'X' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.down
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).down
                            )
                    , Cmd.none
                    )

                Keyboard.Alpha 'W' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.up
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).up
                            )
                    , Cmd.none
                    )

                Keyboard.Alpha 'D' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.right
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).right
                            )
                    , Cmd.none
                    )

                Keyboard.Alpha 'A' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.left
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).left
                            )
                    , Cmd.none
                    )

                Keyboard.Alpha 'E' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.backward
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).forward
                            )
                    , Cmd.none
                    )

                Keyboard.Alpha 'Z' ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.forward
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).backward
                            )
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
            ( model
                |> Model.updateAssetStore (AssetStore.addToStore scale asset model.assets)
            , Cmd.none
            )
