module XYZMika.XYZ.Mesh.Gizmo exposing (..)

import Color
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Cube


axis : Mesh Vertex
axis =
    axisAtPosition (vec3 0 0 0)


axisAtPosition : Vec3 -> Mesh Vertex
axisAtPosition t =
    let
        size =
            0.1

        thickness =
            0.005

        { x, y, z } =
            Vec3.toRecord t

        xBounds =
            ( vec3 x -thickness -thickness
            , vec3 (x + size) thickness thickness
            )

        yBounds =
            ( vec3 -thickness y -thickness
            , vec3 thickness (y + size) thickness
            )

        zBounds =
            ( vec3 -thickness -thickness z
            , vec3 thickness thickness (z + size)
            )
    in
    XYZMika.XYZ.Mesh.Cube.withBoundsAndColor Color.darkRed xBounds
        ++ XYZMika.XYZ.Mesh.Cube.withBoundsAndColor Color.darkGreen yBounds
        ++ XYZMika.XYZ.Mesh.Cube.withBoundsAndColor Color.darkBlue zBounds
        |> WebGL.triangles
