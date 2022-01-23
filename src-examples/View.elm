module View exposing (doc, view)

import Array
import Asset
import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Material
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Model exposing (Hud(..), HudMsg(..), HudObject(..), HudValue(..), Model, Msg(..))
import Tree exposing (Tree)
import WebGL
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.AssetStore as AssetStore
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Light as Light
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


attributionView : Model -> Html msg
attributionView model =
    case Array.get model.currentSceneIndex model.scenes of
        Just Model.NormalMapping ->
            div [ class "scene-info" ]
                [ text <| "Model by "
                , a [ href "https://sketchfab.com/spogna" ] [ text "Andrea Spognetta (Spogna)" ]
                ]

        _ ->
            text ""


sceneView : Texture -> Hud -> Model -> Scene Material.Name -> Html Msg
sceneView defaultTexture (Hud hud) model scene =
    main_ [ class "app" ]
        [ div [ class "app__viewport" ]
            [ attributionView model
            , WebGL.toHtml
                [ width Model.viewport.width
                , height Model.viewport.height
                , id "viewport"
                ]
                (Scene.render
                    [ Light.directional (vec3 -1 1 1) ]
                    model.sceneOptions
                    Model.viewport
                    (Model.getDrag model)
                    model.theta
                    (Model.sceneOptions model)
                    (\tree ->
                        let
                            index =
                                Tuple.first (Tree.label tree)
                        in
                        if model.selectedTreeIndex == Just index then
                            Just { showBoundingBox = True }

                        else
                            Nothing
                    )
                    scene
                    (renderer defaultTexture)
                )
            ]
        , aside
            [ class "app__sidebar"
            , classList [ ( "app__sidebar--expanded", hud.sidebarExpanded ) ]
            ]
            [ sidebarView
                { treeCount = Tree.count (Scene.getGraph scene) }
                model.hud
                (Scene.camera scene)
                (Scene.getGraph scene
                    |> Tree.indexedMap Tuple.pair
                    |> Tree.foldl
                        (\( i, x ) acc ->
                            if model.selectedTreeIndex == Just i then
                                Just x

                            else
                                acc
                        )
                        Nothing
                )
                model
            ]
        ]


renderer :
    Texture
    -> Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Uniforms u
    -> Object Material.Name
    -> WebGL.Entity
renderer fallbackTexture name =
    case name of
        Just materialName ->
            Material.renderer fallbackTexture materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer


sidebarView : { treeCount : Int } -> Hud -> Camera -> Maybe (Object Material.Name) -> Model -> Html Msg
sidebarView { treeCount } (Hud hud) camera selectedObject model =
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
                , p [] [ text <| "Node count: " ++ String.fromInt treeCount ]
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
            , selectedObject
                |> Maybe.map selectedObjectWidget
                |> Maybe.withDefault (text "")
            ]
        , button
            [ onClick (HudMsg ToggleSidebar)
            , class "sidebar__toggle"
            ]
            []
        ]


selectedObjectWidget : Object Material.Name -> Html Msg
selectedObjectWidget object =
    vector3Widget
        (Object.toHumanReadable object)
        SelectedGraph
        (object |> Object.position |> Vec3.toRecord)


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
