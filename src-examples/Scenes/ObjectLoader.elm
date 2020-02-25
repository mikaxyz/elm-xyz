module Scenes.ObjectLoader exposing (addMesh, getObj, init, sceneOptions)

import Http
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene exposing (Options, Scene, defaultScene)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


init : Scene Material.Name
init =
    { defaultScene
        | graph =
            [ Graph
                (XYZMika.XYZ.Mesh.Cube.gray 2 0.1 2
                    |> Object.withMesh
                    |> Object.withPosition (vec3 0 -0.5 0)
                    |> Object.withOptionDragToRotateXY
                )
                []
            ]
        , camera = Mat4.makeLookAt (vec3 0 1 2.5) (vec3 0 0 0) (vec3 0 1 0)
    }


sceneOptions : Maybe Options
sceneOptions =
    Just
        { rotation = always Mat4.identity
        , translate = always Mat4.identity
        , perspective = \aspectRatio -> Mat4.makePerspective 45 aspectRatio 0.01 100
        }


getObj : { scale : Float, color : Vec3 } -> Vec3 -> String -> (( { scale : Float, color : Vec3 }, Vec3, String ) -> msg) -> Cmd msg
getObj options pos url tagger =
    Http.get
        { url = url
        , expect = Http.expectString (\x -> tagger ( options, pos, x |> Result.withDefault "" ))
        }


addMesh : Maybe Material.Name -> List ( Vertex, Vertex, Vertex ) -> Vec3 -> Scene Material.Name -> Scene Material.Name
addMesh material tris pos scene =
    let
        graphObject : Object Material.Name
        graphObject =
            tris
                |> WebGL.triangles
                |> Object.withMesh
                |> (\x -> material |> Maybe.map (\m -> x |> Object.withMaterialName m) |> Maybe.withDefault x)
                |> Object.withPosition (Vec3.add pos (vec3 0 0.05 0))

        updated =
            case scene.graph of
                graph :: _ ->
                    case graph of
                        Graph root children ->
                            [ Graph root (Graph graphObject [] :: children) ]

                _ ->
                    scene.graph
    in
    { scene | graph = updated }
