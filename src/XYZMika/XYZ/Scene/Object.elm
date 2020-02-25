module XYZMika.XYZ.Scene.Object exposing
    ( Object
    , Options
    , diffuseMap
    , diffuseMapWithDefault
    , materialName
    , mesh
    , normalMap
    , normalMapIntensityWithDefault
    , normalMapWithDefault
    , position
    , rotation
    , rotationInTime
    , rotationWithDrag
    , rotationWithDragX
    , rotationWithDragXY
    , rotationWithDragY
    , withDiffuseMap
    , withMaterialName
    , withMesh
    , withNormalMap
    , withNormalMapIntensity
    , withOptionDragToRotateX
    , withOptionDragToRotateXY
    , withOptionDragToRotateY
    , withOptionRotationInTime
    , withOptionTranslateInTime
    , withOptions
    , withPosition
    , withRotation
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material


type Object materialId
    = Mesh (ObjectData materialId)


type alias ObjectData materialId =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , options : Maybe Options
    , diffuseMap : Maybe Texture
    , normalMap : Maybe Texture
    , normalMapIntensity : Maybe Float
    , material : Maybe (Material.Id materialId)
    }


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


mapOptions : (Maybe Options -> Maybe Options) -> Object materialId -> Object materialId
mapOptions f obj =
    case obj of
        Mesh data ->
            Mesh { data | options = f data.options }


withOptionRotationInTime : (Float -> Mat4) -> Object materialId -> Object materialId
withOptionRotationInTime f obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotation = f })
                    |> Maybe.withDefault { defaultOptions | rotation = f }
                    |> Just
            )


withOptionTranslateInTime : (Float -> Mat4) -> Object materialId -> Object materialId
withOptionTranslateInTime f obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | translate = f })
                    |> Maybe.withDefault { defaultOptions | translate = f }
                    |> Just
            )


withOptionDragToRotateX : Object materialId -> Object materialId
withOptionDragToRotateX obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotationWithDrag = rotationWithDragX })
                    |> Maybe.withDefault { defaultOptions | rotationWithDrag = rotationWithDragX }
                    |> Just
            )


withOptionDragToRotateY : Object materialId -> Object materialId
withOptionDragToRotateY obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotationWithDrag = rotationWithDragY })
                    |> Maybe.withDefault { defaultOptions | rotationWithDrag = rotationWithDragY }
                    |> Just
            )


withOptionDragToRotateXY : Object materialId -> Object materialId
withOptionDragToRotateXY obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotationWithDrag = rotationWithDragXY })
                    |> Maybe.withDefault { defaultOptions | rotationWithDrag = rotationWithDragXY }
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


get : (ObjectData materialId -> a) -> Object materialId -> a
get f obj =
    case obj of
        Mesh data ->
            f data



-- READ


position : Object materialId -> Vec3
position obj =
    obj |> get .position


rotation : Object materialId -> Mat4
rotation obj =
    obj |> get .rotation


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


mesh : Object materialId -> Mesh Vertex
mesh obj =
    obj |> get .mesh


materialName : Object materialId -> Maybe (Material.Id materialId)
materialName obj =
    -- TODO: Just return materialName
    obj |> get .material



-- CREATE


mapData : (ObjectData materialId -> ObjectData materialId) -> Object materialId -> Object materialId
mapData f obj =
    case obj of
        Mesh data ->
            Mesh (f data)


withMesh : Mesh Vertex -> Object materialId
withMesh x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , diffuseMap = Nothing
        , normalMap = Nothing
        , normalMapIntensity = Nothing
        , options = Nothing
        , material = Nothing
        }


withOptions : Options -> Object materialId -> Object materialId
withOptions x obj =
    obj |> mapData (\data -> { data | options = Just x })


withMaterialName : materialId -> Object materialId -> Object materialId
withMaterialName x obj =
    obj |> mapData (\data -> { data | material = Just (Material.Id x) })


withDiffuseMap : Texture -> Object materialId -> Object materialId
withDiffuseMap x obj =
    obj |> mapData (\data -> { data | diffuseMap = Just x })


withNormalMap : Texture -> Object materialId -> Object materialId
withNormalMap x obj =
    obj |> mapData (\data -> { data | normalMap = Just x })


withNormalMapIntensity : Float -> Object materialId -> Object materialId
withNormalMapIntensity x obj =
    obj |> mapData (\data -> { data | normalMapIntensity = Just x })


withRotation : Mat4 -> Object materialId -> Object materialId
withRotation x obj =
    obj |> mapData (\data -> { data | rotation = x })


withPosition : Vec3 -> Object materialId -> Object materialId
withPosition x obj =
    obj |> mapData (\data -> { data | position = x })
