module Model exposing (Branch, Model, Msg(..), camera, init)

import DDD.Data.Node exposing (Node(..))
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type Msg
    = Animate Float
    | DragStart Vec2
    | Drag Vec2
    | DragEnd Vec2


type alias Model =
    { theta : Float
    , tree : Node Branch
    , camera : Camera
    , drag : Maybe { from : Vec2, to : Vec2 }
    }


type alias Camera =
    { dolly : Vec2
    , dollySpeed : Float
    }


type alias Branch =
    { l : Float
    , r : Float
    }


camera : Model -> Camera
camera model =
    let
        camera_ =
            model.camera
    in
    model.drag
        |> Maybe.map (\drag -> Vec2.sub drag.from drag.to)
        |> Maybe.map (Vec2.scale model.camera.dollySpeed)
        |> Maybe.map (\distance -> { camera_ | dolly = Vec2.sub camera_.dolly distance })
        |> Maybe.withDefault camera_


createTree : Int -> Int -> Node Branch
createTree i r =
    if i == 1 then
        Node
            (Branch (toFloat i) (toFloat r))
            Empty
            Empty

    else
        Node
            (Branch (toFloat i) (toFloat r))
            (createTree (i - 1) 1)
            (createTree (i - 1) -1)


init : Model
init =
    let
        t =
            createTree 5 0

        --                |> Debug.log "tree"
    in
    { theta = 0
    , tree = t
    , camera = { dolly = vec2 0 0, dollySpeed = 0.008 }
    , drag = Nothing
    }
