module Update exposing (update)

import DDD.Scene as Scene
import Math.Vector2 as Vec2
import Model exposing (Model, Msg(..))



--import Scenes.ObjectLoader


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }, Cmd.none )

        DragStart pos ->
            ( { model | drag = Just { from = pos, to = pos } }, Cmd.none )

        Drag pos ->
            ( { model
                | drag = Maybe.map (\drag -> { drag | to = pos }) model.drag
                , scene =
                    model.drag
                        |> Maybe.map (\drag -> Scene.cameraRotate (Vec2.sub drag.from drag.to |> Vec2.scale 0.01) model.scene)
                        |> Maybe.withDefault model.scene
              }
            , Cmd.none
            )

        DragEnd _ ->
            ( { model
                | drag = Nothing
                , scene = Scene.cameraRotateApply model.scene
              }
            , Cmd.none
            )

        KeyPressed key ->
            case String.toLower key of
                "j" ->
                    model |> Model.nextScene

                "k" ->
                    model |> Model.prevScene

                _ ->
                    ( model, Cmd.none )

        GotObj str ->
            ( model, Cmd.none )



--            ( { model | scene = Scenes.ObjectLoader.addMesh (Scenes.ObjectLoader.mesh str) model.scene }
--            , Cmd.none
--            )
