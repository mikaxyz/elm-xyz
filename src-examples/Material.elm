module Material exposing (..)

import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene.Object exposing (Object)


type Name
    = Simple
    | Advanced


renderer : Name -> Texture -> { u | perspective : Mat4, camera : Mat4, worldMatrix : Mat4 } -> Object materialId -> Entity
renderer name =
    case name of
        Simple ->
            XYZMika.XYZ.Material.Simple.renderer

        Advanced ->
            XYZMika.XYZ.Material.Advanced.renderer
