module Material exposing (Name(..), renderer)

import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Color
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Material.Textured
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Name
    = Simple
    | Color
    | Advanced
    | Textured


renderer :
    Texture
    -> Name
    -> Material.Options
    -> Uniforms u
    -> Object materialId
    -> Entity
renderer fallbackTexture name =
    case name of
        Simple ->
            XYZMika.XYZ.Material.Simple.renderer

        Color ->
            XYZMika.XYZ.Material.Color.renderer

        Advanced ->
            XYZMika.XYZ.Material.Advanced.renderer

        Textured ->
            XYZMika.XYZ.Material.Textured.renderer fallbackTexture
