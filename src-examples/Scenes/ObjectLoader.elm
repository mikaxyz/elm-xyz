module Scenes.ObjectLoader exposing (addMesh, getObj, init, sceneOptions)

import Http
import Material
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Tree
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube
import XYZMika.XYZ.Scene as Scene exposing (Options, Scene)
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


init : Scene Material.Name
init =
    Scene.init { gizmoMaterial = Material.Simple } rootObject
        |> Scene.withCameraPosition (vec3 0 1 2.5)


rootObject =
    Tree.singleton
        (XYZMika.XYZ.Mesh.Cube.withBounds ( vec3 -1 0 -1, vec3 1 0.1 1 )
            |> Object.initWithTriangles
            |> Object.withPosition (vec3 0 -0.5 0)
            |> Object.withOptionDragToRotateXY
        )


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
    tris
        |> Object.initWithTriangles
        |> (\x -> material |> Maybe.map (\m -> x |> Object.withMaterialName m) |> Maybe.withDefault x)
        |> Object.withPosition (Vec3.add pos (vec3 0 0.05 0))
        |> (\x ->
                Scene.map
                    (\graph ->
                        --case graph of
                        --    Graph root children ->
                        --        Graph root (Graph x [] :: children)
                        graph |> Tree.appendChild (Tree.singleton x)
                    )
                    scene
           )
