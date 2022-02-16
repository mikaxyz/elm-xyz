module ShadowMapping.Model exposing
    ( Model
    , Msg(..)
    , init
    , initScene
    , keyboardControl
    , modifiers
    )

import Keyboard
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import ShadowMapping.Assets as Assets exposing (ObjId, TextureId)
import ShadowMapping.Scene as Scene
import XYZMika.Dragon as Dragon exposing (Dragon)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Scene exposing (Scene)
import XYZMika.XYZ.Scene.Graph as Graph
import XYZMika.XYZ.Scene.Object as Object


type Msg
    = Animate Float
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
    | DragonMsg Dragon.Msg
    | DragonOnDrag Dragon.Vector
    | AssetLoaded (Result AssetStore.Error AssetStore.Content)


type alias Model =
    { theta : Float
    , keyboard : Keyboard.State
    , dragon : Dragon
    , assets : AssetStore.Store ObjId TextureId
    , scene : Scene Material.Name
    , objectPosition : Vec3
    }


init : Model
init =
    { theta = 0
    , keyboard = Keyboard.init
    , dragon = Dragon.init
    , assets = AssetStore.init Assets.objPath Assets.texturePath
    , scene =
        XYZMika.XYZ.Scene.init
            (Graph.singleton (Object.group "LAODING"))
            |> XYZMika.XYZ.Scene.withCameraPosition (vec3 0 5 7)
            |> XYZMika.XYZ.Scene.withCameraTarget (vec3 0 0.5 0)
    , objectPosition = vec3 0 0 0
    }


initScene : Model -> Model
initScene model =
    Maybe.map5
        (\mesh diffuse normal carpetDiffuse carpetNormal ->
            { mesh = mesh
            , diffuse = diffuse
            , normal = normal
            , carpetDiffuse = carpetDiffuse
            , carpetNormal = carpetNormal
            }
        )
        (AssetStore.verticesIndexed Assets.SneakerXyz model.assets)
        (AssetStore.texture Assets.SneakerDiffuse model.assets)
        (AssetStore.texture Assets.SneakerNormal model.assets)
        (AssetStore.texture Assets.CarpetDiffuse model.assets)
        (AssetStore.texture Assets.CarpetNormal model.assets)
        |> Maybe.map
            (\assets ->
                { model
                    | scene =
                        model.scene
                            |> XYZMika.XYZ.Scene.map
                                (\_ ->
                                    Scene.graph assets
                                )
                }
            )
        |> Maybe.withDefault model


keyboardControl : Model -> Model
keyboardControl model =
    let
        speed =
            0.05

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


modifiers : Model -> List XYZMika.XYZ.Scene.Modifier
modifiers model =
    [ XYZMika.XYZ.Scene.PositionModifier
        (\index position ->
            case index of
                1 ->
                    Vec3.add model.objectPosition position

                _ ->
                    position
        )
    , XYZMika.XYZ.Scene.RotationModifier
        (\index matrix ->
            case index of
                1 ->
                    matrix
                        |> Mat4.rotate (5 * model.theta) Vec3.j

                _ ->
                    matrix
        )
    , XYZMika.XYZ.Scene.SpotLightTargetModifier
        (\index target ->
            Vec3.add model.objectPosition target
        )
    ]
