module ShadowMapping.Model exposing (Model, Msg(..), init)

import Keyboard
import Math.Vector3 exposing (Vec3, vec3)
import ShadowMapping.Assets as Assets exposing (ObjId, TextureId)
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
            |> XYZMika.XYZ.Scene.withCameraTarget (vec3 0 0.5 0)
    , objectPosition = vec3 0 0 0
    }
