module View exposing (doc, view)

import Asset
import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Material
import Math.Vector3 as Vec3 exposing (Vec3)
import Model exposing (Hud(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm Web GL Experiment"
    , body =
        AssetStore.texture Asset.Empty model.assets
            |> Maybe.map (\defaultTexture -> view defaultTexture model |> List.singleton)
            |> Maybe.withDefault []
    }


type alias Config =
    { width : Int
    , height : Int
    }


viewport =
    { width = 1600
    , height = 800
    }


view : Texture -> Model -> Html Msg
view defaultTexture model =
    model.scene
        |> Maybe.map (sceneView defaultTexture model.hud model)
        |> Maybe.withDefault (text "")


sceneView : Texture -> Hud -> Model -> Scene Material.Name -> Html Msg
sceneView defaultTexture (Hud hud) model scene =
    main_ [ class "app" ]
        [ div [ class "app__viewport" ]
            [ WebGL.toHtml
                [ width viewport.width
                , height viewport.height
                ]
                (Scene.render
                    defaultTexture
                    model.renderOptions
                    viewport
                    (Model.getDrag model)
                    model.theta
                    (Model.sceneOptions model)
                    (scene |> Scene.withRendererOptions model.rendererOptions)
                    renderer
                )
            ]
        , aside
            [ class "app__sidebar"
            , classList [ ( "app__sidebar--expanded", hud.sidebarExpanded ) ]
            ]
            [ sidebarView model.hud model scene
            ]
        ]


renderer :
    Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Texture
    -> Uniforms u
    -> Object Material.Name
    -> WebGL.Entity
renderer name =
    case name of
        Just materialName ->
            Material.renderer materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer


sidebarView : Hud -> Model -> Scene Material.Name -> Html Msg
sidebarView (Hud hud) model scene =
    div
        [ class "sidebar"
        , classList [ ( "sidebar--expanded", hud.sidebarExpanded ) ]
        ]
        [ section
            [ class "sidebar__content"
            ]
            [ header []
                [ h1 [ class "sidebar__title" ] [ text <| Model.currentSceneName model ]
                ]
            , Camera.position (Scene.camera scene)
                |> Vec3.toRecord
                |> vector3Widget "Camera" Camera
            ]
        , button
            [ onClick (HudMsg ToggleSidebar)
            , class "sidebar__toggle"
            ]
            []
        ]


vector3Widget : String -> HudObject -> { x : Float, y : Float, z : Float } -> Html Msg
vector3Widget title hudObject vector =
    section [ class "widget" ]
        [ h3 [] [ text title ]
        , rangeInput "x" hudObject HudValue_Vec3_X vector.x
        , rangeInput "y" hudObject HudValue_Vec3_Y vector.y
        , rangeInput "z" hudObject HudValue_Vec3_Z vector.z
        ]


rangeInput : String -> HudObject -> HudValue -> Float -> Html Msg
rangeInput title hudObject hudValue value_ =
    label [ class "widget__control" ]
        [ span [ class "widget__control-meta" ]
            [ span [] [ text title ]
            , span [] [ text (String.fromFloat value_) ]
            ]
        , input
            [ type_ "range"
            , HA.min "-10"
            , HA.max "10"
            , HA.step "0.0001"
            , value (String.fromFloat value_)
            , onInput (SetValue hudObject hudValue)
            ]
            []
        ]
