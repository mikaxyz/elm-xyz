module Update exposing (update)

import Model exposing (Model, Msg(..))


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
              }
            , Cmd.none
            )

        DragEnd pos ->
            ( { model
                | drag = Nothing
                , camera =
                    Model.camera
                        { model | drag = Maybe.map (\drag -> { drag | to = pos }) model.drag }
              }
            , Cmd.none
            )
