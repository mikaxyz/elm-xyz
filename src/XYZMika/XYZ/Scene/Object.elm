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
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , boundingBox = ( vec3 0 0 0, vec3 0 0 0 )
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
        , boundingBox = getBounds x
        , diffuseMap = Nothing
        , normalMap = Nothing
        , normalMapIntensity = Nothing
        , options = Nothing
        , material = Nothing
        , color = Color.white
        }


initWithIndexedTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> Object materialId
initWithIndexedTriangles ( v, i ) =
    init (WebGL.indexedTriangles v i)


getBounds : List ( Vertex, Vertex, Vertex ) -> ( Vec3, Vec3 )
getBounds vs =
    vs
        |> List.map
            (\( v1, v2, v3 ) ->
                ( Vec3.toRecord v1.position
                , Vec3.toRecord v2.position
                , Vec3.toRecord v3.position
                )
            )
        |> List.foldl
            (\( p1, p2, p3 ) ->
                \( a1, a2 ) ->
                    let
                        xMin =
                            a1.x
                                |> min p1.x
                                |> min p2.x
                                |> min p3.x

                        xMax =
                            a2.x
                                |> max p1.x
                                |> max p2.x
                                |> max p3.x

                        yMin =
                            a1.y
                                |> min p1.y
                                |> min p2.y
                                |> min p3.y

                        yMax =
                            a2.y
                                |> max p1.y
                                |> max p2.y
                                |> max p3.y

                        zMin =
                            a1.z
                                |> min p1.z
                                |> min p2.z
                                |> min p3.z

                        zMax =
                            a2.z
                                |> max p1.z
                                |> max p2.z
                                |> max p3.z
                    in
                    ( { x = xMin, y = yMin, z = zMin }
                    , { x = xMax, y = yMax, z = zMax }
                    )
            )
            ( Vec3.toRecord (vec3 0 0 0), Vec3.toRecord (vec3 0 0 0) )
        |> Tuple.mapBoth Vec3.fromRecord Vec3.fromRecord


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
