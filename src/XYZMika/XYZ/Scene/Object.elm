module XYZMika.XYZ.Scene.Object exposing
    ( Object, init, initWithTriangles
    , withPosition, withRotation, withColor, withMaterialName
    , withDiffuseMap, withNormalMap, withNormalMapIntensity
    , mesh, position, rotation, color, colorVec3, materialName, boundingBox
    , diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault, normalMapIntensityWithDefault
    , withOptionRotationInTime, withOptionDragToRotateX, withOptionDragToRotateXY, withOptionDragToRotateY
    , rotationInTime, rotationWithDrag
    , initWithIndexedTriangles
    )

{-|


# Create

@docs Object, init, initWithTriangles


## Modify

@docs withPosition, withRotation, withColor, withMaterialName
@docs withDiffuseMap, withNormalMap, withNormalMapIntensity


## Read

@docs mesh, position, rotation, color, colorVec3, materialName, boundingBox
@docs diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault, normalMapIntensity, normalMapIntensityWithDefault


# Options (Move these to "Modifier.elm")


## Modify

@docs withOptionRotationInTime, withOptionDragToRotateX, withOptionDragToRotateXY, withOptionDragToRotateY


## Read

@docs rotationInTime, rotationWithDrag

-}

import Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)


type Object materialId
    = Mesh (ObjectData materialId)


type alias ObjectData materialId =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , boundingBox : ( Vec3, Vec3 )
    , options : Maybe Options
    , diffuseMap : Maybe Texture
    , normalMap : Maybe Texture
    , normalMapIntensity : Maybe Float
    , material : Maybe materialId
    , color : Color
    }



-- Create


init : Mesh Vertex -> Object materialId
init x =
    initWithBounds ( vec3 0 0 0, vec3 0 0 0 ) x


initWithBounds : ( Vec3, Vec3 ) -> Mesh Vertex -> Object a
initWithBounds bounds x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , boundingBox = bounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , normalMapIntensity = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        }


initWithTriangles : List ( Vertex, Vertex, Vertex ) -> Object materialId
initWithTriangles x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.triangles x
        , boundingBox =
            x
                |> List.foldl (\( v1, v2, v3 ) acc -> v1 :: v2 :: v3 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , normalMapIntensity = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        }


initWithIndexedTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> Object materialId
initWithIndexedTriangles ( v, i ) =
    initWithBounds (getBounds v) (WebGL.indexedTriangles v i)


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


withNormalMapIntensity : Float -> Object materialId -> Object materialId
withNormalMapIntensity x obj =
    obj |> mapData (\data -> { data | normalMapIntensity = Just x })



-- HELPERS


mapOptions : (Maybe Options -> Maybe Options) -> Object materialId -> Object materialId
mapOptions f obj =
    case obj of
        Mesh data ->
            Mesh { data | options = f data.options }


mapData : (ObjectData materialId -> ObjectData materialId) -> Object materialId -> Object materialId
mapData f obj =
    case obj of
        Mesh data ->
            Mesh (f data)


get : (ObjectData materialId -> a) -> Object materialId -> a
get f obj =
    case obj of
        Mesh data ->
            f data



-- Read


boundingBox : Object materialId -> ( Vec3, Vec3 )
boundingBox obj =
    obj |> get .boundingBox


mesh : Object materialId -> Mesh Vertex
mesh obj =
    obj |> get .mesh


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


normalMapIntensityWithDefault : Float -> Object materialId -> Float
normalMapIntensityWithDefault default obj =
    obj |> get .normalMapIntensity |> Maybe.withDefault default



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
