module Update exposing (update)

import Math.Vector2 as Vec2
import Model exposing (Model, Msg(..))
import Scenes.ObjectLoader
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Parser.Obj
import XYZMika.XYZ.Scene


pointLightDistance =
    3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }, Cmd.none )

        DragStart pos ->
            ( { model | dragger = Just { from = pos, to = pos } }, Cmd.none )

        Drag pos ->
            ( { model
                | dragger = Maybe.map (\drag -> { drag | to = pos }) model.dragger
              }
            , Cmd.none
            )

        DragEnd pos ->
            ( { model
                | dragger = Nothing
                , drag =
                    model.dragger
                        |> Maybe.map (\x -> Vec2.add model.drag (Vec2.sub x.to x.from))
                        |> Maybe.withDefault model.drag
              }
            , Cmd.none
            )

        KeyPressed key ->
            case String.toLower key of
                "x" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.down
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).down
                            )
                    , Cmd.none
                    )

                "w" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.up
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).up
                            )
                    , Cmd.none
                    )

                "d" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.right
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).right
                            )
                    , Cmd.none
                    )

                "a" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.left
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).left
                            )
                    , Cmd.none
                    )

                "e" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.backward
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).forward
                            )
                    , Cmd.none
                    )

                "z" ->
                    ( model
                        |> Model.mapRendererOptions
                            (XYZMika.XYZ.Material.setDirectionalLight XYZMika.XYZ.Scene.direction.forward
                                >> XYZMika.XYZ.Material.setPointLight (XYZMika.XYZ.Scene.inDirection pointLightDistance).backward
                            )
                    , Cmd.none
                    )

                "s" ->
                    ( model |> Model.mapRendererOptions (always XYZMika.XYZ.Material.defaultOptions)
                    , Cmd.none
                    )

                "1" ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showGeometry = not x.showGeometry })
                    , Cmd.none
                    )

                "2" ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showBoundingBoxes = not x.showBoundingBoxes })
                    , Cmd.none
                    )

                "3" ->
                    ( model |> Model.mapRenderOptions (\x -> { x | showBoundingBoxesOverlay = not x.showBoundingBoxesOverlay })
                    , Cmd.none
                    )

                "j" ->
                    model |> Model.nextScene

                "k" ->
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
