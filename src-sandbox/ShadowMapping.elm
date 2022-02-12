module ShadowMapping exposing (main)

import Browser
import Browser.Events
import Keyboard
import Math.Vector3 as Vec3 exposing (vec3)
import ShadowMapping.Assets as Assets
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
            Model.init
                |> (\model ->
                        ( model
                        , Cmd.batch
                            [ AssetStore.loadXyz Assets.SneakerXyz model.assets AssetLoaded
                            , AssetStore.loadTexture Assets.SneakerNormal model.assets AssetLoaded
                            , AssetStore.loadTexture Assets.SneakerDiffuse model.assets AssetLoaded
                            ]
                        )
                   )
                |> always
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


keyboardControl model =
    let
        speed =
            0.3

        x =
            if model.keyboard |> Keyboard.isKeyDown (Keyboard.Alpha 'D') then
                1.0

            else if model.keyboard |> Keyboard.isKeyDown (Keyboard.Alpha 'A') then
                -1.0

            else
                0.0

        y =
            if model.keyboard |> Keyboard.isKeyDown (Keyboard.Alpha 'S') then
                1.0

            else if model.keyboard |> Keyboard.isKeyDown (Keyboard.Alpha 'W') then
                -1.0

            else
                0.0

        m =
            vec3 x 0 y
    in
    { model | objectPosition = model.objectPosition |> Vec3.add (Vec3.scale speed m) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate elapsed ->
            ( { model | theta = model.theta + (elapsed / 10000) } |> keyboardControl
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
