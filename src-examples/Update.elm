module Update exposing (update)

import Math.Vector2 as Vec2
import Model exposing (Model, Msg(..))
import Scenes.ObjectLoader
import Scenes.Textures
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Parser.Obj


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
                "j" ->
                    model |> Model.nextScene

                "k" ->
                    model |> Model.prevScene

                _ ->
                    ( model, Cmd.none )

        GotObj ( options, pos, str ) ->
            ( { model
                | scene =
                    Scenes.ObjectLoader.addMesh
                        (XYZMika.XYZ.Parser.Obj.parse options str)
                        pos
                        model.scene
              }
            , Cmd.none
            )

        AssetLoaded scale asset ->
            ( model
                |> (\m -> { m | assets = m.assets |> AssetStore.addToStore scale asset })
                |> (\m -> { m | scene = Scenes.Textures.init m.assets })
            , Cmd.none
            )



--{ model | assets = model.assets |> Asset.Store.addToStore asset }
--