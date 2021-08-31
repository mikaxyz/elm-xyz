module XYZMika.XYZ.Scene.Object exposing
    ( Object, initWithTriangles, initWithLines, initWithIndexedTriangles, light, pointLight
    , withPosition, withRotation, withColor, withMaterialName, withGlSetting
    , withDiffuseMap, withNormalMap
    , mesh, triangles, position, rotation, color, colorVec3, materialName, boundingBox, glSetting
    , diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault
    , withOptionRotationInTime, withOptionDragToRotateX, withOptionDragToRotateXY, withOptionDragToRotateY, toHumanReadable
    , rotationInTime, rotationWithDrag, maybeLight
    , disable, enable, group, isDisabled, maybeGroup, maybeLightDisabled
    )

{-|


# Create

@docs Object, initWithTriangles, initWithLines, initWithIndexedTriangles, light, pointLight


## Modify

@docs withPosition, withRotation, withColor, withMaterialName, withGlSetting
@docs withDiffuseMap, withNormalMap


## Read

@docs mesh, triangles, position, rotation, color, colorVec3, materialName, boundingBox, glSetting
@docs diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault


# Options (Move these to "Modifier.elm")


## Modify

@docs withOptionRotationInTime, withOptionDragToRotateX, withOptionDragToRotateXY, withOptionDragToRotateY, toEmpty, toHumanReadable


## Read

@docs rotationInTime, rotationWithDrag, maybeLight

-}

import Array exposing (Array)
import Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import WebGL.Settings
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube as Cube
import XYZMika.XYZ.Scene.Light as Light exposing (Light)


type Object materialId
    = Disabled (Object materialId)
    | Mesh (ObjectData materialId)
    | Light (ObjectData materialId) Light
    | Group String (ObjectData materialId)


toHumanReadable : Object materialId -> String
toHumanReadable object =
    case object of
        Disabled _ ->
            "Disabled"

        Mesh _ ->
            "Mesh"

        Light _ light_ ->
            Light.toHumanReadable light_

        Group name _ ->
            "Group <" ++ name ++ ">"


type alias ObjectData materialId =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , triangles : List ( Vec3, Vec3, Vec3 )
    , boundingBox : ( Vec3, Vec3 )
    , options : Maybe Options
    , diffuseMap : Maybe Texture
    , normalMap : Maybe Texture
    , material : Maybe materialId
    , color : Color
    , glSetting : Maybe WebGL.Settings.Setting
    }


disable : Object materialId -> Object materialId
disable object =
    case object of
        Disabled x ->
            Disabled x

        Mesh objectData ->
            Disabled (Mesh objectData)

        Light objectData light_ ->
            Disabled (Light objectData light_)

        Group name data ->
            Disabled (Group name data)


enable : Object materialId -> Object materialId
enable object =
    case object of
        Disabled x ->
            x

        Mesh objectData ->
            Mesh objectData

        Light objectData light_ ->
            Light objectData light_

        Group name data ->
            Group name data


pointLight : Float -> Vec3 -> Vec3 -> Object materialId
pointLight intensity position_ color_ =
    light position_
        (Light.pointLight (Vec3.vec3 0 0 0)
            |> Light.withIntensity intensity
            |> Light.withColor color_
        )


group : String -> Object materialId
group name =
    Group name
        { position = vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = [] |> WebGL.triangles
        , triangles = []
        , boundingBox = ( vec3 0 0 0, vec3 0 0 0 )
        , diffuseMap = Nothing
        , normalMap = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        }


light : Vec3 -> Light -> Object materialId
light v light_ =
    let
        size =
            0.2

        verts =
            Cube.gray size size size
    in
    Light
        { position = v
        , rotation = Mat4.identity
        , mesh = verts |> WebGL.triangles
        , triangles = verts |> List.map toVec3s
        , boundingBox = verts |> List.concatMap (\( v1, v2, v3 ) -> [ v1, v2, v3 ]) |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        }
        light_


maybeLight : Object materialId -> Maybe Light
maybeLight object =
    case object of
        Disabled _ ->
            Nothing

        Mesh _ ->
            Nothing

        Light _ light_ ->
            Just light_

        Group name data ->
            Nothing


maybeLightDisabled : Object materialId -> Maybe Light
maybeLightDisabled object =
    case object of
        Disabled object_ ->
            maybeLight object_

        Mesh _ ->
            Nothing

        Light _ _ ->
            Nothing

        Group _ _ ->
            Nothing


maybeGroup : String -> Object materialId -> Maybe (Object materialId)
maybeGroup name object =
    case object of
        Disabled _ ->
            Nothing

        Mesh _ ->
            Nothing

        Light _ _ ->
            Nothing

        Group name_ data ->
            if name_ == name then
                Just (Group name_ data)

            else
                Nothing


isDisabled : Object materialId -> Bool
isDisabled object =
    case object of
        Disabled _ ->
            True

        Mesh _ ->
            False

        Light _ _ ->
            False

        Group _ _ ->
            False


initWithBounds : ( Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 ) -> Mesh Vertex -> Object a
initWithBounds bounds tris x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , triangles = tris
        , boundingBox = bounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        }


initWithLines : List ( Vertex, Vertex ) -> Object materialId
initWithLines x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.lines x
        , triangles = []
        , boundingBox =
            x
                |> List.foldl (\( v1, v2 ) acc -> v1 :: v2 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        }


initWithTriangles : List ( Vertex, Vertex, Vertex ) -> Object materialId
initWithTriangles x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.triangles x
        , triangles = x |> List.map toVec3s
        , boundingBox =
            x
                |> List.foldl (\( v1, v2, v3 ) acc -> v1 :: v2 :: v3 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        }


initWithIndexedTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> Object materialId
initWithIndexedTriangles ( v, i ) =
    initWithBounds (getBounds v) (toTriangles ( v, i )) (WebGL.indexedTriangles v i)


toVec3s : ( Vertex, Vertex, Vertex ) -> ( Vec3, Vec3, Vec3 )
toVec3s ( v1, v2, v3 ) =
    ( v1.position, v2.position, v3.position )


toTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> List ( Vec3, Vec3, Vec3 )
toTriangles ( vertices, indices ) =
    let
        vs : Array Vertex
        vs =
            Array.fromList vertices
    in
    indices
        |> List.filterMap
            (\( i1, i2, i3 ) ->
                Maybe.map3
                    (\v1 v2 v3 -> toVec3s ( v1, v2, v3 ))
                    (Array.get i1 vs)
                    (Array.get i2 vs)
                    (Array.get i3 vs)
            )


getBounds : List Vertex -> ( Vec3, Vec3 )
getBounds vs =
    vs
        |> List.map .position
        |> List.foldl
            (\v ( min, max ) ->
                ( vMin min v, vMax max v )
            )
            ( vec3 0 0 0, vec3 0 0 0 )


vMin : Vec3 -> Vec3 -> Vec3
vMin v1 v2 =
    let
        ( x, y, z ) =
            ( min (Vec3.getX v1) (Vec3.getX v2)
            , min (Vec3.getY v1) (Vec3.getY v2)
            , min (Vec3.getZ v1) (Vec3.getZ v2)
            )
    in
    vec3 x y z


vMax : Vec3 -> Vec3 -> Vec3
vMax v1 v2 =
    let
        ( x, y, z ) =
            ( max (Vec3.getX v1) (Vec3.getX v2)
            , max (Vec3.getY v1) (Vec3.getY v2)
            , max (Vec3.getZ v1) (Vec3.getZ v2)
            )
    in
    vec3 x y z


withGlSetting : WebGL.Settings.Setting -> Object materialId -> Object materialId
withGlSetting x object =
    case object of
        Disabled object_ ->
            Disabled object_

        Mesh objectData ->
            Mesh { objectData | glSetting = Just x }

        Light objectData light_ ->
            Light { objectData | glSetting = Just x } light_

        Group name data ->
            Group name { data | glSetting = Just x }


glSetting : Object materialId -> Maybe WebGL.Settings.Setting
glSetting object =
    case object of
        Disabled object_ ->
            glSetting object_

        Mesh objectData ->
            objectData.glSetting

        Light objectData _ ->
            objectData.glSetting

        Group _ data ->
            data.glSetting


withPosition : Vec3 -> Object materialId -> Object materialId
withPosition x obj =
    obj |> mapData (\data -> { data | position = x })


withRotation : Mat4 -> Object materialId -> Object materialId
withRotation x obj =
    obj |> mapData (\data -> { data | rotation = x })


withColor : Color -> Object materialId -> Object materialId
withColor x obj =
    obj |> mapData (\data -> { data | color = x })


withMaterialName : materialId -> Object materialId -> Object materialId
withMaterialName x obj =
    obj |> mapData (\data -> { data | material = Just x })


withDiffuseMap : Texture -> Object materialId -> Object materialId
withDiffuseMap x obj =
    obj |> mapData (\data -> { data | diffuseMap = Just x })


withNormalMap : Texture -> Object materialId -> Object materialId
withNormalMap x obj =
    obj |> mapData (\data -> { data | normalMap = Just x })



-- HELPERS


mapOptions : (Maybe Options -> Maybe Options) -> Object materialId -> Object materialId
mapOptions f obj =
    case obj of
        Disabled obj_ ->
            Disabled obj_

        Mesh data ->
            Mesh { data | options = f data.options }

        Light data light_ ->
            Light { data | options = f data.options } light_

        Group name data ->
            Group name { data | options = f data.options }


mapData : (ObjectData materialId -> ObjectData materialId) -> Object materialId -> Object materialId
mapData f obj =
    case obj of
        Disabled obj_ ->
            Disabled obj_

        Mesh data ->
            Mesh (f data)

        Light data light_ ->
            Light (f data) light_

        Group name data ->
            Group name (f data)


get : (ObjectData materialId -> a) -> Object materialId -> a
get f obj =
    case obj of
        Disabled obj_ ->
            get f obj_

        Mesh data ->
            f data

        Light data _ ->
            f data

        Group name data ->
            f data



-- Read


boundingBox : Object materialId -> ( Vec3, Vec3 )
boundingBox obj =
    obj |> get .boundingBox


mesh : Object materialId -> Mesh Vertex
mesh obj =
    obj |> get .mesh


triangles : Object materialId -> List ( Vec3, Vec3, Vec3 )
triangles obj =
    obj |> get .triangles


position : Object materialId -> Vec3
position obj =
    obj |> get .position


rotation : Object materialId -> Mat4
rotation obj =
    obj |> get .rotation


color : Object materialId -> Color
color obj =
    obj |> get .color


colorVec3 : Object materialId -> Vec3
colorVec3 obj =
    obj
        |> get .color
        |> Color.toRgba
        |> (\{ red, green, blue } -> vec3 red green blue)


materialName : Object materialId -> Maybe materialId
materialName obj =
    obj |> get .material


diffuseMap : Object materialId -> Maybe Texture
diffuseMap obj =
    obj |> get .diffuseMap


diffuseMapWithDefault : Texture -> Object materialId -> Texture
diffuseMapWithDefault default obj =
    obj |> get .diffuseMap |> Maybe.withDefault default


normalMap : Object materialId -> Maybe Texture
normalMap obj =
    obj |> get .normalMap


normalMapWithDefault : Texture -> Object materialId -> Texture
normalMapWithDefault default obj =
    obj |> get .normalMap |> Maybe.withDefault default



--


rotationWithDrag : Vec2 -> Object materialId -> Object materialId
rotationWithDrag drag obj =
    obj
        |> get .options
        |> Maybe.map
            (\x ->
                obj
                    |> mapData
                        (\data ->
                            { data
                                | rotation = Mat4.mul data.rotation (x.rotationWithDrag drag)
                            }
                        )
            )
        |> Maybe.withDefault obj


rotationInTime : Float -> Object materialId -> Object materialId
rotationInTime theta obj =
    obj
        |> get .options
        |> Maybe.map
            (\x ->
                obj
                    |> mapData
                        (\data ->
                            { data
                                | rotation = Mat4.mul data.rotation (x.rotation theta)
                            }
                        )
            )
        |> Maybe.withDefault obj



-- OPTIONS (Move these to "Modifier.elm")


type alias Options =
    { rotation : Float -> Mat4
    , translate : Float -> Mat4
    , rotationWithDrag : Vec2 -> Mat4
    }


defaultOptions : Options
defaultOptions =
    { rotation = always Mat4.identity
    , translate = always Mat4.identity
    , rotationWithDrag = always Mat4.identity
    }


withOptionRotationInTime : (Float -> Mat4) -> Object materialId -> Object materialId
withOptionRotationInTime f obj =
    obj
        |> mapOptions
            (\options ->
                Maybe.withDefault defaultOptions options
                    |> (\x -> { x | rotation = f })
                    |> Just
            )



--


withOptionDragToRotateX : Object materialId -> Object materialId
withOptionDragToRotateX obj =
    obj
        |> mapOptions
            (\options ->
                Maybe.withDefault defaultOptions options
                    |> (\x -> { x | rotationWithDrag = rotationWithDragX })
                    |> Just
            )


withOptionDragToRotateY : Object materialId -> Object materialId
withOptionDragToRotateY obj =
    obj
        |> mapOptions
            (\options ->
                Maybe.withDefault defaultOptions options
                    |> (\x -> { x | rotationWithDrag = rotationWithDragY })
                    |> Just
            )


withOptionDragToRotateXY : Object materialId -> Object materialId
withOptionDragToRotateXY obj =
    obj
        |> mapOptions
            (\options ->
                Maybe.withDefault defaultOptions options
                    |> (\x -> { x | rotationWithDrag = rotationWithDragXY })
                    |> Just
            )


rotationWithDragXY : Vec2 -> Mat4
rotationWithDragXY drag =
    Mat4.identity
        |> Mat4.rotate (Vec2.getY drag * 0.01) (vec3 1 0 0)
        |> Mat4.rotate (Vec2.getX drag * 0.01) (vec3 0 1 0)


rotationWithDragX : Vec2 -> Mat4
rotationWithDragX drag =
    Mat4.identity
        |> Mat4.rotate (Vec2.getX drag * 0.01) (vec3 0 1 0)


rotationWithDragY : Vec2 -> Mat4
rotationWithDragY drag =
    Mat4.identity
        |> Mat4.rotate (Vec2.getY drag * 0.01) (vec3 1 0 0)
