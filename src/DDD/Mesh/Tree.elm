module DDD.Mesh.Tree exposing (tree)

import DDD.Data.Color as Color exposing (Color)
import DDD.Data.Node exposing (Node(..))
import DDD.Mesh.Primitives exposing (bone2)
import DDD.Scene.Graph exposing (Graph(..))
import DDD.Scene.Object exposing (Object)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import WebGL exposing (..)


branchMultiplier =
    0.05


tree : Int -> Int -> List Graph
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


treeFromNodes : Float -> List Graph -> Node Branch -> List Graph
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


object : Float -> Branch -> Object
object t branch =
    { position = vec3 0 0 0
    , rotation =
        Mat4.mul
            (Mat4.makeTranslate (vec3 0 t 0))
            (Mat4.makeRotate branch.r (vec3 0.4 0.5 0.2))
    , mesh = bone2 Color.orange Color.blue Color.green (0.1 * branch.l) branch.l |> WebGL.triangles
    }
