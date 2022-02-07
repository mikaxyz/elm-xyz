module ShadowMapping exposing (main)

import Browser
import Browser.Events
import Keyboard
import Math.Vector3 as Vec3
import ShadowMapping.Model as Model exposing (Model, Msg(..))
import ShadowMapping.View as View
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Scene as XYZScene
import XYZMika.XYZ.Scene.Camera as Camera


main : Program () Model Msg
main =
    Browser.document
        { init =
            always
                ( Model.init
                , Cmd.none
                )
        , view = View.doc
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Animate
        , Keyboard.subscriptions { tagger = KeyboardMsg, keyDown = OnKeyDown }
        , Dragon.subscriptions model.dragon |> Sub.map DragonMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) }
            , Cmd.none
            )

        AssetLoaded (Ok asset) ->
            ( { model | assets = model.assets |> AssetStore.addToStore asset }, Cmd.none )

        AssetLoaded (Err error) ->
            let
                _ =
                    Debug.log "AssetLoaded" error
            in
            ( model, Cmd.none )

        KeyboardMsg msg_ ->
            ( { model | keyboard = Keyboard.update msg_ model.keyboard }
            , Cmd.none
            )

        OnKeyDown key ->
            ( model, Cmd.none )

        DragonMsg msg_ ->
            Dragon.update { tagger = DragonMsg, onDragUpdate = DragonOnDrag } msg_ model.dragon
                |> Tuple.mapFirst (\dragon -> { model | dragon = dragon })

        DragonOnDrag drag ->
            ( { model
                | scene =
                    model.scene
                        |> XYZScene.withCameraMap
                            (\camera ->
                                camera
                                    |> Camera.withOrbitY -(drag.x / 200)
                                    |> Camera.withPositionMap
                                        (\position ->
                                            position
                                                |> Vec3.setY
                                                    (Vec3.getY position + (drag.y / 20))
                                        )
                            )
              }
            , Cmd.none
            )
