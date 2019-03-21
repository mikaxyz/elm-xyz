module Scenes.Light exposing (init, sceneOptions)

import DDD.Data.Vertex exposing (Vertex)
import DDD.Mesh.Cube
import DDD.Scene as Scene exposing (Options, Scene, defaultScene)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object as Object
import DDD.Scene.Uniforms exposing (Uniforms)
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Shader)


opt =
    { rotation = \theta -> Mat4.makeRotate (10 * theta) (vec3 0 1 0)
    , translate = always Mat4.identity
    }


init : Scene
init =
    { defaultScene
        | graph =
            [ Graph
                (DDD.Mesh.Cube.gray 1.4 1.4 1.4
                    |> Object.withMesh
                    |> Object.withOptions opt
                    |> Object.withVertexShader vertexShader
                    |> Object.withFragmentShader fragmentShader
                )
                []
            , light Scene.lightPosition1
            , light Scene.lightPosition2
            ]
        , camera = Mat4.makeLookAt (vec3 0 2 6) (vec3 0 0 0) (vec3 0 1 0)
    }


light p =
    Graph
        (DDD.Mesh.Cube.mesh 0.02 0.02 0.02
            |> Object.withMesh
            |> Object.withPosition p
            |> Object.withFragmentShader lightFragmentShader
        )
        []


sceneOptions : Maybe Options
sceneOptions =
    --    Nothing
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }



--
--vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3 }
--vertexShader =
--    [glsl|
--        attribute vec3 position;
--        attribute vec3 color;
--        attribute vec3 normal;
--
--        uniform mat4 perspective;
--        uniform mat4 camera;
--        uniform mat4 rotation;
--        uniform mat4 translate;
--        varying vec3 vcolor;
--        varying vec3 vnormal;
--
--        vec3 sun = vec3(0.5, 1.0, 1.5);
--
--        void main () {
--            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
--            vcolor = color;
--            vnormal = normal;
--        }
--    |]
--vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3 }
--vertexShader =
--    [glsl|
--        attribute vec3 position;
--        attribute vec3 color;
--        uniform mat4 perspective;
--        uniform mat4 camera;
--        uniform mat4 rotation;
--        uniform mat4 translate;
--        varying vec3 vcolor;
--        varying vec3 vnormal;
--
--        vec3 campos = camera[3].xyz;
--
--        void main () {
--            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
--            vcolor = mix(normalize(campos), normalize(position), 0.8);
--        }
--    |]
--fragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3 }
--fragmentShader =
--    [glsl|
--        precision mediump float;
--        uniform float shade;
--        varying vec3 vcolor;
--        varying vec3 vnormal;
--
--        vec3 sun = vec3(0.5, 3.0, 2);
--
--        void main () {
--            vec3 light = dot(normalize(vnormal), normalize(sun));
--            // gl_FragColor = texture(tex, f_texcoord) * vec4(light, 1.0);
--            gl_FragColor = vec4(light, 1.0);
--        }
--    |]


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 normal;
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translate;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;
        //varying vec3 vnormal;
        //f_position = vec3(mvp * vec4(position, 1.0));

        void main () {
            gl_Position = perspective * camera * rotation * translate * vec4(position, 1.0);
            vcolor = color;
            vnormal = normal;
            vposition = position;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float shade;
        uniform vec3 light1;
        uniform vec3 light2;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;

        // vec3 sun = light1;
        float lightintensity1 = 0.0;
        float lightintensity2 = 0.0;

        void main () {
            vec3 l1 = normalize(light1 - vposition);
            vec3 l2 = normalize(light2 - vposition);
            //light = max(dot(vnormal, light), 0.0) * vec3(1.0, 1.0, 1.0);

            //lightintensity1 = dot(vnormal, l1);
            //lightintensity2 = dot(vnormal, l2);
            lightintensity1 = max(dot(vnormal, l1), 0.0);
            lightintensity2 = max(dot(vnormal, l2), 0.0);

            //gl_FragColor = shade * vec4(vcolor, light);
            gl_FragColor = (lightintensity1 + lightintensity2) * vec4(vcolor, 1.0);

            //vec3 light = normalize(sun);
            //gl_FragColor = shade * vec4(vcolor, light);
        }
    |]



--


lightFragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vposition : Vec3 }
lightFragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;

        varying vec3 vcolor;
        varying vec3 vnormal;
        varying vec3 vposition;

        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
