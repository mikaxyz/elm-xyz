module XYZMika.XYZ.Scene.Options exposing
    ( Option
    , Options
    , create
    , showBoundingBoxes
    , showBoundingBoxesOption
    , showBoundingBoxesOverlay
    , showBoundingBoxesOverlayOption
    , showGeometry
    , showGeometryOption
    , showGridX
    , showGridXOption
    , showGridY
    , showGridYOption
    , showGridZ
    , showGridZOption
    , showLightGizmos
    , showLightGizmosOption
    , toggle
    )


type Option
    = ShowGeometry
    | ShowBoundingBoxes
    | ShowBoundingBoxesOverlay
    | ShowLightGizmos
    | ShowGridX
    | ShowGridY
    | ShowGridZ


showGeometry : Options -> Bool
showGeometry (Options x) =
    x.showGeometry


showBoundingBoxes : Options -> Bool
showBoundingBoxes (Options x) =
    x.showBoundingBoxes


showBoundingBoxesOverlay : Options -> Bool
showBoundingBoxesOverlay (Options x) =
    x.showBoundingBoxesOverlay


showLightGizmos : Options -> Bool
showLightGizmos (Options x) =
    x.showLightGizmos


showGridX : Options -> Bool
showGridX (Options x) =
    x.showGridX


showGridY : Options -> Bool
showGridY (Options x) =
    x.showGridY


showGridZ : Options -> Bool
showGridZ (Options x) =
    x.showGridZ


showGeometryOption : Option
showGeometryOption =
    ShowGeometry


showBoundingBoxesOption : Option
showBoundingBoxesOption =
    ShowBoundingBoxes


showBoundingBoxesOverlayOption : Option
showBoundingBoxesOverlayOption =
    ShowBoundingBoxesOverlay


showLightGizmosOption : Option
showLightGizmosOption =
    ShowLightGizmos


showGridXOption : Option
showGridXOption =
    ShowGridX


showGridYOption : Option
showGridYOption =
    ShowGridY


showGridZOption : Option
showGridZOption =
    ShowGridZ


type Options
    = Options
        { showGeometry : Bool
        , showBoundingBoxes : Bool
        , showBoundingBoxesOverlay : Bool
        , showLightGizmos : Bool
        , showGridX : Bool
        , showGridY : Bool
        , showGridZ : Bool
        }


create : Options
create =
    Options
        { showGeometry = True
        , showBoundingBoxes = False
        , showBoundingBoxesOverlay = False
        , showLightGizmos = False
        , showGridX = False
        , showGridY = True
        , showGridZ = False
        }


toggle : Option -> Options -> Options
toggle option (Options options) =
    Options
        { showGeometry = toggle_ option ShowGeometry not options.showGeometry
        , showBoundingBoxes = toggle_ option ShowBoundingBoxes not options.showBoundingBoxes
        , showBoundingBoxesOverlay = toggle_ option ShowBoundingBoxesOverlay not options.showBoundingBoxesOverlay
        , showLightGizmos = toggle_ option ShowLightGizmos not options.showLightGizmos
        , showGridX = toggle_ option ShowGridX not options.showGridX
        , showGridY = toggle_ option ShowGridY not options.showGridY
        , showGridZ = toggle_ option ShowGridZ not options.showGridZ
        }


toggle_ : Option -> Option -> (a -> a) -> a -> a
toggle_ option1 option2 f x =
    if option1 == option2 then
        f x

    else
        x
