module XYZMika.XYZ.Mesh.Tree exposing (tree)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)
import XYZMika.Color as Color
import XYZMika.XYZ.Data.Node exposing (Node(..))
import XYZMika.XYZ.Mesh.Primitives exposing (bone2)
import XYZMika.XYZ.Scene.Graph exposing (Graph(..))
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


branchMultiplier =
    0.05


tree : Int -> Int -> List (Graph materialId)
tree i r =
    nodes i r
        |> treeFromNodes 0 []


nodes : Int -> Int -> Node Branch
nodes i r =
    if i == 1 then
        Node
            (Branch (toFloat i * branchMultiplier) (toFloat r))
            Empty
            Empty

    else
        Node
            (Branch (toFloat i * branchMultiplier) (toFloat r))
            (nodes (i - 1) 1)
            (nodes (i - 1) -1)


treeFromNodes : Float -> List (Graph materialId) -> Node Branch -> List (Graph materialId)
treeFromNodes y t node =
    case node of
        Empty ->
            t

        Node branch left right ->
            [ Graph (object y branch)
                (List.append
                    (treeFromNodes branch.l [] left)
                    (treeFromNodes branch.l [] right)
                )
            ]


type alias Branch =
    { l : Float
    , r : Float
    }


object : Float -> Branch -> Object materialId
object t branch =
    bone2 Color.orange Color.blue Color.green (0.3 * branch.l) branch.l
        |> WebGL.triangles
        |> Object.init
        |> Object.withPosition (vec3 0 0 0)
        |> Object.withRotation
            (Mat4.mul
                (Mat4.makeTranslate (vec3 0 t 0))
                (Mat4.makeRotate branch.r (vec3 0.4 0.5 0.2))
            )
