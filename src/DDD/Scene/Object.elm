module DDD.Scene.Object exposing
    ( Object
    , Options
    , fragmentShader
    , mesh
    , normalMap
    , normalMapWithDefault
    , position
    , rotation
    , rotationInTime
    , rotationWithDrag
    , rotationWithDragX
    , rotationWithDragXY
    , rotationWithDragY
    , textureMap
    , textureWithDefault
    , vertexShader
    , withFragmentShader
    , withMesh
    , withNormalMap
    , withOptionDragToRotateX
    , withOptionDragToRotateXY
    , withOptionDragToRotateY
    , withOptionRotationInTime
    , withOptionTranslateInTime
    , withOptions
    , withPosition
    , withRotation
    , withTexture
    , withVertexShader
    )

import DDD.Data.Vertex exposing (Vertex)
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


type Object
    = Mesh ObjectData Shaders


type alias VertexShader =
    Shader Vertex Uniforms Varyings


type alias FragmentShader =
    Shader {} Uniforms Varyings


type alias ObjectData =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , options : Maybe Options
    , texture : Maybe Texture
    , normalMap : Maybe Texture
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


mapOptions : (Maybe Options -> Maybe Options) -> Object -> Object
mapOptions f obj =
    case obj of
        Mesh data shaders ->
            Mesh { data | options = f data.options } shaders


withOptionRotationInTime : (Float -> Mat4) -> Object -> Object
withOptionRotationInTime f obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotation = f })
                    |> Maybe.withDefault { defaultOptions | rotation = f }
                    |> Just
            )


withOptionTranslateInTime : (Float -> Mat4) -> Object -> Object
withOptionTranslateInTime f obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | translate = f })
                    |> Maybe.withDefault { defaultOptions | translate = f }
                    |> Just
            )


withOptionDragToRotateX : Object -> Object
withOptionDragToRotateX obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotationWithDrag = rotationWithDragX })
                    |> Maybe.withDefault { defaultOptions | rotationWithDrag = rotationWithDragX }
                    |> Just
            )


withOptionDragToRotateY : Object -> Object
withOptionDragToRotateY obj =
    obj
        |> mapOptions
            (\options ->
                options
                    |> Maybe.map (\x -> { x | rotationWithDrag = rotationWithDragY })
                    |> Maybe.withDefault { defaultOptions | rotationWithDrag = rotationWithDragY }
                    |> Just
            )


withOptionDragToRotateXY : Object -> Object
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


type alias Shaders =
    { vertexShader : Maybe VertexShader
    , fragmentShader : Maybe FragmentShader
    }


get : (ObjectData -> a) -> Object -> a
get f obj =
    case obj of
        Mesh data _ ->
            f data



-- READ


position : Object -> Vec3
position obj =
    obj |> get .position


rotation : Object -> Mat4
rotation obj =
    obj |> get .rotation


textureMap : Object -> Maybe Texture
textureMap obj =
    obj |> get .texture


textureWithDefault : Texture -> Object -> Texture
textureWithDefault default obj =
    obj |> get .texture |> Maybe.withDefault default


normalMap : Object -> Maybe Texture
normalMap obj =
    obj |> get .normalMap


normalMapWithDefault : Texture -> Object -> Texture
normalMapWithDefault default obj =
    obj |> get .normalMap |> Maybe.withDefault default


rotationWithDrag : Vec2 -> Object -> Object
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


rotationInTime : Float -> Object -> Object
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


mesh : Object -> Mesh Vertex
mesh obj =
    obj |> get .mesh


vertexShader : Object -> Maybe VertexShader
vertexShader obj =
    case obj of
        Mesh _ shaders ->
            shaders.vertexShader


fragmentShader : Object -> Maybe FragmentShader
fragmentShader obj =
    case obj of
        Mesh _ shaders ->
            shaders.fragmentShader



-- CREATE


mapData : (ObjectData -> ObjectData) -> Object -> Object
mapData f obj =
    case obj of
        Mesh data shaders ->
            Mesh (f data) shaders


mapShaders : (Shaders -> Shaders) -> Object -> Object
mapShaders f obj =
    case obj of
        Mesh data shaders ->
            Mesh data (f shaders)


withMesh : Mesh Vertex -> Object
withMesh x =
    Mesh
        { position = Vec3.vec3 0 0 0
        , rotation = Mat4.identity
        , mesh = x
        , texture = Nothing
        , normalMap = Nothing
        , options = Nothing
        }
        { vertexShader = Nothing
        , fragmentShader = Nothing
        }


withOptions : Options -> Object -> Object
withOptions x obj =
    obj |> mapData (\data -> { data | options = Just x })


withTexture : Texture -> Object -> Object
withTexture x obj =
    obj |> mapData (\data -> { data | texture = Just x })


withNormalMap : Texture -> Object -> Object
withNormalMap x obj =
    obj |> mapData (\data -> { data | normalMap = Just x })


withRotation : Mat4 -> Object -> Object
withRotation x obj =
    obj |> mapData (\data -> { data | rotation = x })


withPosition : Vec3 -> Object -> Object
withPosition x obj =
    obj |> mapData (\data -> { data | position = x })


withVertexShader : VertexShader -> Object -> Object
withVertexShader x obj =
    obj |> mapShaders (\data -> { data | vertexShader = Just x })


withFragmentShader : FragmentShader -> Object -> Object
withFragmentShader x obj =
    obj |> mapShaders (\data -> { data | fragmentShader = Just x })
