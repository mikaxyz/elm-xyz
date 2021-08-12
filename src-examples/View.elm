module View exposing (doc, view)

import Asset
import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Material
import Math.Vector3 as Vec3 exposing (Vec3)
import Model exposing (Hud(..), HudLightObject(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Light as Light exposing (PointLight)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)
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
                [ width Model.viewport.width
                , height Model.viewport.height
                , id "viewport"
                ]
                (Scene.render
                    defaultTexture
                    model.renderOptions
                    Model.viewport
                    (Model.getDrag model)
                    model.theta
                    (Model.sceneOptions model)
                    (\graph ->
                        if model.selectedGraph == Just graph then
                            Just { showBoundingBox = True }

                        else
                            Nothing
                    )
                    scene
                    renderer
                )
            ]
        , aside
            [ class "app__sidebar"
            , classList [ ( "app__sidebar--expanded", hud.sidebarExpanded ) ]
            ]
            [ sidebarView model.hud
                (Scene.camera scene)
                (Scene.pointLights scene)
                model
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


sidebarView : Hud -> Camera -> List PointLight -> Model -> Html Msg
sidebarView (Hud hud) camera pointLights model =
    div
        [ class "sidebar"
        , classList [ ( "sidebar--expanded", hud.sidebarExpanded ) ]
        , onClickFinal (HudMsg Click)
        ]
        [ section
            [ class "sidebar__content"
            ]
            [ header []
                [ h1 [ class "sidebar__title" ] [ text <| Model.currentSceneName model ]
                ]
            , Camera.position camera
                |> Vec3.toRecord
                |> vector3Widget "Camera" Camera

            --, div [ class "value" ]
            --    [ span [ class "value__title" ] [ text "Roll" ]
            --    , Camera.roll camera
            --        --|> (*) 180
            --        |> valueToHtml
            --    , text "°"
            --    ]
            -- TODO: Make this input work. Store other value?
            , rangeInput "Roll" Camera HudValue_Vec3_Roll -1 1 True (Camera.roll camera)
            , model.selectedGraph
                |> Maybe.map selectedGraphWidget
                |> Maybe.withDefault (pointLightWidgets pointLights)
            ]
        , button
            [ onClick (HudMsg ToggleSidebar)
            , class "sidebar__toggle"
            ]
            []
        ]


selectedGraphWidget : Graph (Object Material.Name) -> Html Msg
selectedGraphWidget (Graph object _) =
    vector3Widget "Object" SelectedGraph (Object.position object |> Vec3.toRecord)


pointLightWidgets lights =
    lights
        |> List.indexedMap pointLightControl
        |> div []


pointLightControl : Int -> PointLight -> Html Msg
pointLightControl index light =
    let
        lightHudObject : Maybe HudLightObject
        lightHudObject =
            case index of
                0 ->
                    Just PointLight1

                1 ->
                    Just PointLight2

                _ ->
                    Nothing
    in
    lightHudObject
        |> Maybe.map
            (\x ->
                Light.position light
                    |> Vec3.toRecord
                    |> vector3Widget ("Point light " ++ String.fromInt (index + 1)) (LightHudObject x)
            )
        |> Maybe.withDefault (text "")


valueToHtml x =
    formatNumber x
        |> (\( sign, value ) -> span [] [ text sign, text value ])


formatNumber x =
    formatNumberWithDecimals 3 x


formatNumberWithDecimals : Int -> Float -> ( String, String )
formatNumberWithDecimals decimals x =
    let
        sign =
            if x < 0.0 then
                "-"

            else
                "+"
    in
    case String.fromFloat (abs x) |> String.split "." of
        int :: [] ->
            ( sign, int ++ "." ++ (List.repeat decimals '0' |> String.fromList) )

        int :: [ rest ] ->
            ( sign, int ++ "." ++ String.left decimals rest )

        _ ->
            ( "", "NaN" )


vector3Widget : String -> HudObject -> { x : Float, y : Float, z : Float } -> Html Msg
vector3Widget title hudObject vector =
    section [ class "widget" ]
        [ h3 [] [ text title ]
        , rangeInput "x" hudObject HudValue_Vec3_X -10 10 False vector.x
        , rangeInput "y" hudObject HudValue_Vec3_Y -10 10 False vector.y
        , rangeInput "z" hudObject HudValue_Vec3_Z -10 10 False vector.z
        ]


rangeInput : String -> HudObject -> HudValue -> Float -> Float -> Bool -> Float -> Html Msg
rangeInput title hudObject hudValue min_ max_ isDisabled value_ =
    label [ class "widget__control" ]
        [ span [ class "value" ]
            [ span [ class "value__title" ] [ text title ]
            , span [] [ valueToHtml value_ ]
            ]
        , input
            [ type_ "range"
            , HA.min (String.fromFloat min_)
            , HA.max (String.fromFloat max_)
            , HA.step "0.0001"
            , value (String.fromFloat value_)
            , onInput (SetValue hudObject hudValue)
            , disabled isDisabled
            ]
            []
        ]


onClickFinal : msg -> Attribute msg
onClickFinal msg =
    JD.succeed { message = msg, stopPropagation = True, preventDefault = False }
        |> Html.Events.custom "mousedown"
