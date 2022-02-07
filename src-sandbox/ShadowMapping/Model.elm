module ShadowMapping.Model exposing (Model, Msg(..), init)

import Keyboard
import Math.Vector2 exposing (Vec2)
import ShadowMapping.Assets as Assets exposing (ObjId, TextureId)
import ShadowMapping.Scene as Scene
import XYZMika.Dragon as Dragon exposing (Dragon)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material.Renderer as Material
import XYZMika.XYZ.Scene exposing (Scene)


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
    }


init : Model
init =
    { theta = 0
    , keyboard = Keyboard.init
    , dragon = Dragon.init
    , assets = AssetStore.init Assets.objPath Assets.texturePath
    , scene = Scene.graph Nothing |> XYZMika.XYZ.Scene.init
    }
