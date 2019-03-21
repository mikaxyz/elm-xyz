module DDD.Scene.Object exposing
    ( Object
    , Options
    , fragmentShader
    , mesh
    , position
    , rotation
    , rotationInTime
    , vertexShader
    , withFragmentShader
    , withMesh
    , withOptions
    , withPosition
    , withRotation
    , withVertexShader
    )

import DDD.Data.Vertex exposing (Vertex)
import DDD.Scene.Uniforms exposing (Uniforms)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Mesh, Shader)


type Object
    = Mesh ObjectData Shaders


type alias VertexShader =
    Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }


type alias FragmentShader =
    Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }


type alias ObjectData =
    { position : Vec3
    , rotation : Mat4
    , mesh : Mesh Vertex
    , options : Maybe Options
    }


type alias Options =
    { rotation : Float -> Mat4
    , translate : Float -> Mat4
    }


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


rotationInTime : Float -> Object -> Mat4
rotationInTime theta obj =
    obj
        |> get .options
        |> Maybe.map
            (\x ->
                x.rotation theta
            )
        |> Maybe.withDefault (obj |> get .rotation)


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
        , options = Nothing
        }
        { vertexShader = Nothing
        , fragmentShader = Nothing
        }


withOptions : Options -> Object -> Object
withOptions x obj =
    obj |> mapData (\data -> { data | options = Just x })


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
