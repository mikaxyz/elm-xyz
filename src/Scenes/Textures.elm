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
                (DDD.Mesh.Cube.gray 1.5 0.2 1.5
                    |> Object.withMesh
                    |> Object.withFragmentShader fragmentShader
                    |> Object.withVertexShader vertexShader
                    |> Object.withPosition (vec3 0 -0.5 0)
                    |> Object.withOptionDragToRotateXY
                )
                (getAssets assets |> Maybe.map renderBall |> Maybe.withDefault [])
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 1.5) (vec3 0 0.3 0) (vec3 0 1 0)
    }


renderBall : Config -> List Graph
renderBall config =
    [ config.mesh
        |> Object.withMesh
        |> Object.withTexture config.diffuse
        |> Object.withFragmentShader fragmentShader
        |> Object.withVertexShader vertexShader
        |> Object.withPosition (vec3 0 0.1 0)
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
    }


getAssets : Store Asset.Obj Asset.Texture -> Maybe Config
getAssets assets =
    Maybe.map2
        Config
        (Asset.Store.mesh Asset.Ball assets)
        (Asset.Store.texture Asset.BallDiffuse assets)


sceneOptions : Maybe Options
sceneOptions =
    Nothing


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        attribute vec2 uv;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 translate;
        uniform mat4 rotation;
        uniform mat4 worldMatrix;
        uniform vec3 directionalLight;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * translate * rotation * vec4(position, 1.0);
            
            highp vec3 ambientLight = vec3(0, 0, 0);
            highp vec3 directionalLightColor = vec3(1, 1, 1);
            highp vec3 directionalVector = normalize(directionalLight);
            highp vec4 transformedNormal = worldMatrix * vec4(normal, 0.0);
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);

            vlighting = ambientLight + (directionalLightColor * directional);
            
            vcolor = color;
            vcoord = uv;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        
        uniform sampler2D texture;


        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        varying vec3 vlighting;
        varying vec2 vcoord;

        void main () {
        
            vec4 tex = texture2D(texture, vcoord);
//            gl_FragColor = vec4(vcolor * vlighting, 1.0);

            gl_FragColor = tex * vec4(vcolor * vlighting , 1.0);
        }
    |]
