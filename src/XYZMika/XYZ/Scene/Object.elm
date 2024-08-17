module XYZMika.XYZ.Scene.Object exposing
    ( Object, initWithTriangles, initWithLines, initWithIndexedTriangles, light
    , withPosition, withRotation, withColor, withMaterialName, withGlSetting
    , withDiffuseMap, withNormalMap
    , withName
    , mesh, triangles, position, rotation, color, colorVec3, materialName, boundingBox, glSetting
    , diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault
    , toHumanReadable
    , maybeLight
    , BoneTransforms, boneTransforms, boneTransformsIdentity, withBoneTransforms
    , disable, enable, group, groupWithId, id, isDisabled, lightTargetMap, map, maybeGroup, maybeLightDisabled, objectObjectWithIndexedTriangles, objectWithTriangles, spotLight, spotLightWithId, withLightTarget
    )

{-|


# Create

@docs Object, initWithTriangles, initWithLines, initWithIndexedTriangles, light, pointLight


## Modify

@docs withPosition, withRotation, withColor, withMaterialName, withGlSetting
@docs withDiffuseMap, withNormalMap
@docs withName


## Read

@docs mesh, triangles, position, rotation, color, colorVec3, materialName, boundingBox, glSetting
@docs diffuseMap, diffuseMapWithDefault, normalMap, normalMapWithDefault


## Modify

@docs toEmpty, toHumanReadable


## Read

@docs maybeLight


## Skeleton

@docs BoneTransforms, boneTransforms, boneTransformsIdentity, withBoneTransforms

-}

import Array exposing (Array)
import Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import WebGL.Settings
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube as Cube
import XYZMika.XYZ.Scene.Light as Light exposing (Light)
import XYZMika.XYZ.Scene.Light.SpotLight as SpotLight exposing (SpotLight)


type Object id materialId
    = Disabled (Object id materialId)
    | Mesh (ObjectData id materialId)
    | Light (ObjectData id materialId) Light
    | Group String (ObjectData id materialId)


toHumanReadable : Object id materialId -> String
toHumanReadable object =
    case object of
        Disabled _ ->
            "[Disabled] " ++ toHumanReadable object

        Mesh data ->
            "Mesh <" ++ Maybe.withDefault "Untitled" data.name ++ ">"

        Light _ light_ ->
            Light.toHumanReadable light_

        Group name _ ->
            "Group <" ++ name ++ ">"


type alias ObjectData id materialId =
    { id : Maybe id
    , name : Maybe String
    , position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , triangles : List ( Vec3, Vec3, Vec3 )
    , boundingBox : ( Vec3, Vec3 )
    , diffuseMap : Maybe Texture
    , normalMap : Maybe Texture
    , material : Maybe materialId
    , color : Color
    , glSetting : Maybe WebGL.Settings.Setting
    , boneTransforms : Maybe BoneTransforms
    }


type alias Skin =
    { inverseBindMatrices : List Mat4
    , joints : List Int
    }


type alias BoneTransforms =
    { joint0 : Mat4
    , joint1 : Mat4
    , joint2 : Mat4
    , joint3 : Mat4
    , joint4 : Mat4
    , joint5 : Mat4
    , joint6 : Mat4
    , joint7 : Mat4
    , joint8 : Mat4
    , joint9 : Mat4
    , joint10 : Mat4
    , joint11 : Mat4
    , joint12 : Mat4
    , joint13 : Mat4
    , joint14 : Mat4
    , joint15 : Mat4
    , joint16 : Mat4
    , joint17 : Mat4
    , joint18 : Mat4
    , joint19 : Mat4
    , joint20 : Mat4
    , joint21 : Mat4
    , joint22 : Mat4
    , joint23 : Mat4
    , joint24 : Mat4
    , inverseBindMatrix0 : Mat4
    , inverseBindMatrix1 : Mat4
    , inverseBindMatrix2 : Mat4
    , inverseBindMatrix3 : Mat4
    , inverseBindMatrix4 : Mat4
    , inverseBindMatrix5 : Mat4
    , inverseBindMatrix6 : Mat4
    , inverseBindMatrix7 : Mat4
    , inverseBindMatrix8 : Mat4
    , inverseBindMatrix9 : Mat4
    , inverseBindMatrix10 : Mat4
    , inverseBindMatrix11 : Mat4
    , inverseBindMatrix12 : Mat4
    , inverseBindMatrix13 : Mat4
    , inverseBindMatrix14 : Mat4
    , inverseBindMatrix15 : Mat4
    , inverseBindMatrix16 : Mat4
    , inverseBindMatrix17 : Mat4
    , inverseBindMatrix18 : Mat4
    , inverseBindMatrix19 : Mat4
    , inverseBindMatrix20 : Mat4
    , inverseBindMatrix21 : Mat4
    , inverseBindMatrix22 : Mat4
    , inverseBindMatrix23 : Mat4
    , inverseBindMatrix24 : Mat4
    }


boneTransformsIdentity : BoneTransforms
boneTransformsIdentity =
    { joint0 = Mat4.identity
    , joint1 = Mat4.identity
    , joint2 = Mat4.identity
    , joint3 = Mat4.identity
    , joint4 = Mat4.identity
    , joint5 = Mat4.identity
    , joint6 = Mat4.identity
    , joint7 = Mat4.identity
    , joint8 = Mat4.identity
    , joint9 = Mat4.identity
    , joint10 = Mat4.identity
    , joint11 = Mat4.identity
    , joint12 = Mat4.identity
    , joint13 = Mat4.identity
    , joint14 = Mat4.identity
    , joint15 = Mat4.identity
    , joint16 = Mat4.identity
    , joint17 = Mat4.identity
    , joint18 = Mat4.identity
    , joint19 = Mat4.identity
    , joint20 = Mat4.identity
    , joint21 = Mat4.identity
    , joint22 = Mat4.identity
    , joint23 = Mat4.identity
    , joint24 = Mat4.identity
    , inverseBindMatrix0 = Mat4.identity
    , inverseBindMatrix1 = Mat4.identity
    , inverseBindMatrix2 = Mat4.identity
    , inverseBindMatrix3 = Mat4.identity
    , inverseBindMatrix4 = Mat4.identity
    , inverseBindMatrix5 = Mat4.identity
    , inverseBindMatrix6 = Mat4.identity
    , inverseBindMatrix7 = Mat4.identity
    , inverseBindMatrix8 = Mat4.identity
    , inverseBindMatrix9 = Mat4.identity
    , inverseBindMatrix10 = Mat4.identity
    , inverseBindMatrix11 = Mat4.identity
    , inverseBindMatrix12 = Mat4.identity
    , inverseBindMatrix13 = Mat4.identity
    , inverseBindMatrix14 = Mat4.identity
    , inverseBindMatrix15 = Mat4.identity
    , inverseBindMatrix16 = Mat4.identity
    , inverseBindMatrix17 = Mat4.identity
    , inverseBindMatrix18 = Mat4.identity
    , inverseBindMatrix19 = Mat4.identity
    , inverseBindMatrix20 = Mat4.identity
    , inverseBindMatrix21 = Mat4.identity
    , inverseBindMatrix22 = Mat4.identity
    , inverseBindMatrix23 = Mat4.identity
    , inverseBindMatrix24 = Mat4.identity
    }


map : ({ position : Vec3, rotation : Mat4 } -> { position : Vec3, rotation : Mat4 }) -> Object a b -> Object a b
map f object =
    let
        update x =
            f { position = x.position, rotation = x.rotation }
    in
    case object of
        Disabled object_ ->
            Disabled object_

        Mesh data ->
            Mesh { data | position = (update data).position, rotation = (update data).rotation }

        Light data light_ ->
            Light { data | position = (update data).position, rotation = (update data).rotation } light_

        Group name data ->
            Group name { data | position = (update data).position, rotation = (update data).rotation }


disable : Object id materialId -> Object id materialId
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


enable : Object id materialId -> Object id materialId
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


group : String -> Object id materialId
group name =
    groupWithId_ Nothing name


groupWithId : id -> String -> Object id materialId
groupWithId id_ name =
    groupWithId_ (Just id_) name


groupWithId_ : Maybe id -> String -> Object id materialId
groupWithId_ id_ name =
    Group name
        { id = id_
        , name = Just name
        , position = vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = [] |> WebGL.triangles
        , triangles = []
        , boundingBox = ( vec3 0 0 0, vec3 0 0 0 )
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


light : Light -> Object id materialId
light light_ =
    let
        size =
            1

        verts =
            Cube.gray size size size
    in
    Light
        { id = Nothing
        , name = Just (Light.toHumanReadable light_)
        , position = Light.position light_ |> Maybe.withDefault (vec3 0 0 0)
        , rotation = Mat4.identity
        , mesh = verts |> WebGL.triangles
        , triangles = verts |> List.map toVec3s
        , boundingBox = verts |> List.concatMap (\( v1, v2, v3 ) -> [ v1, v2, v3 ]) |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }
        light_


spotLight : SpotLight -> Object id materialId
spotLight light_ =
    spotLightWithId_ Nothing light_


spotLightWithId : id -> SpotLight -> Object id materialId
spotLightWithId objectId light_ =
    spotLightWithId_ (Just objectId) light_


spotLightWithId_ : Maybe id -> SpotLight -> Object id materialId
spotLightWithId_ objectId light_ =
    let
        size =
            1

        verts =
            Cube.gray size size size
    in
    Light
        { id = objectId
        , name = Nothing
        , position = SpotLight.position light_
        , rotation = Mat4.identity
        , mesh = verts |> WebGL.triangles
        , triangles = verts |> List.map toVec3s
        , boundingBox = verts |> List.concatMap (\( v1, v2, v3 ) -> [ v1, v2, v3 ]) |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }
        (Light.fromSpotLight light_)


maybeLight : Object id materialId -> Maybe Light
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


maybeLightDisabled : Object id materialId -> Maybe Light
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


maybeGroup : String -> Object id materialId -> Maybe (Object id materialId)
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


isDisabled : Object id materialId -> Bool
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


initWithBounds : ( Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 ) -> Mesh Vertex -> Object id a
initWithBounds bounds tris x =
    Mesh
        { id = Nothing
        , name = Nothing
        , position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , triangles = tris
        , boundingBox = bounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


initWithLines : List ( Vertex, Vertex ) -> Object id materialId
initWithLines x =
    Mesh
        { id = Nothing
        , name = Nothing
        , position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.lines x
        , triangles = []
        , boundingBox =
            x
                |> List.foldl (\( v1, v2 ) acc -> v1 :: v2 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


initWithTriangles : List ( Vertex, Vertex, Vertex ) -> Object id materialId
initWithTriangles x =
    Mesh
        { id = Nothing
        , name = Nothing
        , position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.triangles x
        , triangles = x |> List.map toVec3s
        , boundingBox =
            x
                |> List.foldl (\( v1, v2, v3 ) acc -> v1 :: v2 :: v3 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


initWithIndexedTriangles : ( List Vertex, List ( Int, Int, Int ) ) -> Object id materialId
initWithIndexedTriangles ( v, i ) =
    initWithBounds (getBounds v) (toTriangles ( v, i )) (WebGL.indexedTriangles v i)


objectWithTriangles : id -> List ( Vertex, Vertex, Vertex ) -> Object id materialId
objectWithTriangles objectId x =
    Mesh
        { id = Just objectId
        , name = Nothing
        , position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.triangles x
        , triangles = x |> List.map toVec3s
        , boundingBox =
            x
                |> List.foldl (\( v1, v2, v3 ) acc -> v1 :: v2 :: v3 :: acc) []
                |> getBounds
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


objectObjectWithIndexedTriangles : id -> ( List Vertex, List ( Int, Int, Int ) ) -> Object id materialId
objectObjectWithIndexedTriangles objectId ( v, i ) =
    Mesh
        { id = Just objectId
        , name = Nothing
        , position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = WebGL.indexedTriangles v i
        , triangles = toTriangles ( v, i )
        , boundingBox = getBounds v
        , diffuseMap = Nothing
        , normalMap = Nothing
        , material = Nothing
        , color = Color.white
        , glSetting = Nothing
        , boneTransforms = Nothing
        }


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


withGlSetting : WebGL.Settings.Setting -> Object id materialId -> Object id materialId
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


glSetting : Object id materialId -> Maybe WebGL.Settings.Setting
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


id : Object id materialId -> Maybe id
id object =
    case object of
        Disabled object_ ->
            id object_

        Mesh data ->
            data.id

        Light data _ ->
            data.id

        Group _ data ->
            data.id


boneTransforms : Object id materialId -> Maybe BoneTransforms
boneTransforms object =
    case object of
        Disabled disabledObject ->
            boneTransforms disabledObject

        Mesh data ->
            data.boneTransforms

        Light data light_ ->
            data.boneTransforms

        Group name data ->
            data.boneTransforms


withBoneTransforms : BoneTransforms -> Object id materialId -> Object id materialId
withBoneTransforms x object =
    case object of
        Disabled disabledObject ->
            Disabled (disabledObject |> withBoneTransforms x)

        Mesh data ->
            Mesh { data | boneTransforms = Just x }

        Light data light_ ->
            Light { data | boneTransforms = Just x } light_

        Group name data ->
            Group name { data | boneTransforms = Just x }


withPosition : Vec3 -> Object id materialId -> Object id materialId
withPosition x object =
    case object of
        Disabled disabledObject ->
            Disabled (withPosition x disabledObject)

        Mesh data ->
            Mesh { data | position = x }

        Light data light_ ->
            Light { data | position = x } (Light.withPosition x light_)

        Group name data ->
            Group name { data | position = x }


withLightTarget : Vec3 -> Object id materialId -> Object id materialId
withLightTarget x object =
    case object of
        Disabled disabledObject ->
            Disabled (withPosition x disabledObject)

        Mesh data ->
            Mesh { data | position = x }

        Light data light_ ->
            Light { data | position = x } (Light.withTarget x light_)

        Group name data ->
            Group name { data | position = x }


lightTargetMap : (Vec3 -> Vec3) -> Object id materialId -> Object id materialId
lightTargetMap f object =
    case object of
        Disabled disabledObject ->
            Disabled (lightTargetMap f disabledObject)

        Mesh data ->
            Mesh data

        Light data light_ ->
            Light data (Light.targetMap f light_)

        Group name data ->
            Group name data


withName : String -> Object id materialId -> Object id materialId
withName x obj =
    obj |> mapData (\data -> { data | name = Just x })


withRotation : Mat4 -> Object id materialId -> Object id materialId
withRotation x obj =
    obj |> mapData (\data -> { data | rotation = x })


withColor : Color -> Object id materialId -> Object id materialId
withColor x obj =
    obj |> mapData (\data -> { data | color = x })


withMaterialName : materialId -> Object id materialId -> Object id materialId
withMaterialName x obj =
    obj |> mapData (\data -> { data | material = Just x })


withDiffuseMap : Texture -> Object id materialId -> Object id materialId
withDiffuseMap x obj =
    obj |> mapData (\data -> { data | diffuseMap = Just x })


withNormalMap : Texture -> Object id materialId -> Object id materialId
withNormalMap x obj =
    obj |> mapData (\data -> { data | normalMap = Just x })



-- HELPERS


mapData : (ObjectData id materialId -> ObjectData id materialId) -> Object id materialId -> Object id materialId
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


get : (ObjectData id materialId -> a) -> Object id materialId -> a
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


boundingBox : Object id materialId -> ( Vec3, Vec3 )
boundingBox obj =
    obj |> get .boundingBox


mesh : Object id materialId -> Mesh Vertex
mesh obj =
    obj |> get .mesh


triangles : Object id materialId -> List ( Vec3, Vec3, Vec3 )
triangles obj =
    obj |> get .triangles


position : Object id materialId -> Vec3
position obj =
    obj |> get .position


rotation : Object id materialId -> Mat4
rotation obj =
    obj |> get .rotation


color : Object id materialId -> Color
color obj =
    obj |> get .color


colorVec3 : Object id materialId -> Vec3
colorVec3 obj =
    obj
        |> get .color
        |> Color.toRgba
        |> (\{ red, green, blue } -> vec3 red green blue)


materialName : Object id materialId -> Maybe materialId
materialName obj =
    obj |> get .material


diffuseMap : Object id materialId -> Maybe Texture
diffuseMap obj =
    obj |> get .diffuseMap


diffuseMapWithDefault : Texture -> Object id materialId -> Texture
diffuseMapWithDefault default obj =
    obj |> get .diffuseMap |> Maybe.withDefault default


normalMap : Object id materialId -> Maybe Texture
normalMap obj =
    obj |> get .normalMap


normalMapWithDefault : Texture -> Object id materialId -> Texture
normalMapWithDefault default obj =
    obj |> get .normalMap |> Maybe.withDefault default
