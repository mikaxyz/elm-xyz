module Scenes.Textures exposing (init, renderBall, sceneOptions)

import Asset
import Asset.Store exposing (Store)
import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import DDD.Scene.Uniforms exposing (Uniforms)
import DDD.Scene.Varyings exposing (Varyings)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)



--


init : Store Asset.Obj Asset.Texture -> Scene
init assets =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.gray 12 0.2 12
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -0.5 0)
                    |> Object.withOptionDragToRotateXY
                )
                (getAssets assets |> Maybe.map renderBall |> Maybe.withDefault [])
            ]
        , camera = Mat4.makeLookAt (vec3 0 0.5 3) (vec3 0 0.5 0) (vec3 0 1 0)
    }


renderBall : Config -> List Graph
renderBall config =
    [ config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withPosition (vec3 0.7 0.1 0)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withNormalMap config.normal
        |> Object.withPosition (vec3 0 0.1 0)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withNormalMap config.normal
        |> Object.withPosition (vec3 -0.7 0.1 0)
        |> (\x -> Graph x [])
    , config.treeMesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.treeDiffuse
        |> Object.withPosition (vec3 0 0 -6)
        |> (\x -> Graph x [])
    , config.mesh
        |> Object.withMesh
        |> Object.withDiffuseMap config.diffuse
        |> Object.withNormalMap config.normal
        |> Object.withNormalMapIntensity 10.0
        |> Object.withPosition (vec3 1 0.1 -5)
        |> Object.withOptionRotationInTime
            (\theta ->
                let
                    r =
                        sin (theta * 60)

                    y =
                        r
                in
                Mat4.makeTranslate3 0 (abs y * 0.5) 0
            )
        |> (\x -> Graph x [])
    ]


type alias Config =
    { mesh : Mesh Vertex
    , diffuse : Texture
    , normal : Texture
    , treeMesh : Mesh Vertex
    , treeDiffuse : Texture
    }


getAssets : Store Asset.Obj Asset.Texture -> Maybe Config
getAssets assets =
    Maybe.map5
        Config
        (Asset.Store.mesh Asset.Ball assets)
        (Asset.Store.texture Asset.BallDiffuse assets)
        (Asset.Store.texture Asset.BallNormal assets)
        (Asset.Store.mesh Asset.Tree assets)
        (Asset.Store.texture Asset.TreeDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing
