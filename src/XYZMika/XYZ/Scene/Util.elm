module XYZMika.XYZ.Scene.Util exposing (selectGraphAtClickPosition)

import Browser.Dom as Dom
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Tree exposing (Tree)
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera
import XYZMika.XYZ.Scene.Object as Object exposing (Object)


type alias Viewport =
    { width : Float
    , height : Float
    }


selectGraphAtClickPosition :
    { theta : Float
    , viewport : Viewport
    , viewPortElement : Dom.Element
    }
    -> List (Scene.Modifier objectId materialId)
    -> Scene.Scene objectId materialId
    -> ( Float, Float )
    -> Maybe ( Int, Tree (Object objectId materialId) )
selectGraphAtClickPosition config modifiers scene pos =
    scene
        |> Scene.withModifiers modifiers
        |> selectGraphAtClickPosition_ config pos


selectGraphAtClickPosition_ :
    { theta : Float
    , viewport : Viewport
    , viewPortElement : Dom.Element
    }
    -> ( Float, Float )
    -> Scene.Scene objectId materialId
    -> Maybe ( Int, Tree (Object objectId materialId) )
selectGraphAtClickPosition_ { theta, viewport, viewPortElement } pos scene =
    let
        clickPosition =
            getClickPosition
                viewport
                viewPortElement
                scene
                pos

        graphWithHitInfo : Tree ( Maybe TriangleHitByRay, Int, Object objectId materialId )
        graphWithHitInfo =
            Scene.getGraph scene
                |> Scene.graphWithMatrix { theta = theta, mat = Mat4.identity }
                |> Tree.indexedMap
                    (\index ( mat, object ) ->
                        ( objectClickedInScene clickPosition mat scene object, index, object )
                    )

        hitDistance : Maybe ( Int, TriangleHitByRay )
        hitDistance =
            Tree.flatten graphWithHitInfo
                |> List.filterMap (\( d, index, _ ) -> d |> Maybe.map (\d_ -> ( index, d_ )))
                |> List.sortBy (\( _, TriangleHitByRay distance _ ) -> distance)
                |> List.head

        getGraphByDistance :
            Tree ( Maybe TriangleHitByRay, Int, Object objectId materialId )
            -> ( Int, TriangleHitByRay )
            -> Maybe ( Int, Tree (Object objectId materialId) )
        getGraphByDistance graph ( index, distance ) =
            let
                ( distance_, _, _ ) =
                    Tree.label graph

                children : List (Tree ( Maybe TriangleHitByRay, Int, Object objectId materialId ))
                children =
                    Tree.children graph
            in
            if distance_ == Just distance then
                Just ( index, Tree.map (\( _, _, obj ) -> obj) graph )

            else
                children
                    |> List.foldl
                        (\g_ acc ->
                            let
                                ( _, childIndex, _ ) =
                                    Tree.label g_
                            in
                            case getGraphByDistance g_ ( childIndex, distance ) of
                                Just x ->
                                    Just x

                                Nothing ->
                                    acc
                        )
                        Nothing
    in
    hitDistance
        |> Maybe.andThen (getGraphByDistance graphWithHitInfo)



--


type ClickPosition
    = ClickPosition Vec3


objectClickedInScene_ : ClickPosition -> Mat4 -> Scene objectId materialId -> Object objectId materialId -> Bool
objectClickedInScene_ (ClickPosition clickPosition) transform scene object =
    let
        origin : Vec3
        origin =
            scene
                |> Scene.camera
                |> Camera.position

        direction : Vec3
        direction =
            Vec3.direction clickPosition origin

        triangles =
            Object.triangles object
                |> List.map
                    (\( v0, v1, v2 ) ->
                        ( Mat4.transform transform v0
                        , Mat4.transform transform v1
                        , Mat4.transform transform v2
                        )
                    )
    in
    rayIntersectsTriangles origin direction triangles


objectClickedInScene : ClickPosition -> Mat4 -> Scene objectId materialId -> Object objectId materialId -> Maybe TriangleHitByRay
objectClickedInScene (ClickPosition clickPosition) transform scene object =
    let
        origin : Vec3
        origin =
            scene
                |> Scene.camera
                |> Camera.position

        direction : Vec3
        direction =
            Vec3.direction clickPosition origin

        triangles =
            Object.triangles object
                |> List.map
                    (\( v0, v1, v2 ) ->
                        ( Mat4.transform transform v0
                        , Mat4.transform transform v1
                        , Mat4.transform transform v2
                        )
                    )
    in
    closestTriangleHitByRay origin direction triangles


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


type TriangleHitByRay
    = TriangleHitByRay Float Triangle


closestTriangleHitByRay : Vec3 -> Vec3 -> List ( Vec3, Vec3, Vec3 ) -> Maybe TriangleHitByRay
closestTriangleHitByRay origin destination list =
    list
        |> List.foldl
            (\triangle a ->
                -- TODO: This is not even checking for distance. :) Try overlapping meshes...
                case a of
                    Just previous ->
                        Just previous

                    Nothing ->
                        rayIntersectsTriangle origin destination triangle
            )
            Nothing


rayIntersectsTriangle : Vec3 -> Vec3 -> ( Vec3, Vec3, Vec3 ) -> Maybe TriangleHitByRay
rayIntersectsTriangle origin destination triangle =
    let
        intersect : ( Vec3, Vec3, Vec3 ) -> Maybe Vec3
        intersect =
            rayTriangleIntersect origin destination
    in
    intersect triangle
        |> Maybe.map (\v -> TriangleHitByRay (Vec3.distance origin v) triangle)


rayIntersectsTriangles : Vec3 -> Vec3 -> List ( Vec3, Vec3, Vec3 ) -> Bool
rayIntersectsTriangles origin destination list =
    let
        intersect =
            rayTriangleIntersect origin destination
    in
    List.any
        (\triangle ->
            intersect triangle
                |> (\m ->
                        case m of
                            Nothing ->
                                False

                            Just _ ->
                                True
                   )
        )
        list


rayTriangleIntersect : Vec3 -> Vec3 -> ( Vec3, Vec3, Vec3 ) -> Maybe Vec3
rayTriangleIntersect rayOrigin rayDirection ( triangle0, triangle1, triangle2 ) =
    let
        epsilon =
            0.000001

        edge1 =
            Vec3.sub triangle1 triangle0

        edge2 =
            Vec3.sub triangle2 triangle0

        pvec =
            Vec3.cross rayDirection edge2

        det =
            Vec3.dot edge1 pvec
    in
    if det < epsilon then
        Nothing

    else
        let
            tvec =
                Vec3.sub rayOrigin triangle0

            u =
                Vec3.dot tvec pvec
        in
        if u < 0 || u > det then
            Nothing

        else
            let
                qvec =
                    Vec3.cross tvec edge1

                v =
                    Vec3.dot rayDirection qvec
            in
            if v < 0 || u + v > det then
                Nothing

            else
                let
                    t =
                        Vec3.dot edge2 qvec / det

                    v0 =
                        Vec3.getX rayOrigin + t * Vec3.getX rayDirection

                    v1 =
                        Vec3.getY rayOrigin + t * Vec3.getY rayDirection

                    v2 =
                        Vec3.getZ rayOrigin + t * Vec3.getZ rayDirection
                in
                Just (vec3 v0 v1 v2)


getClickPosition : Viewport -> Dom.Element -> Scene objectId materialId -> ( Float, Float ) -> ClickPosition
getClickPosition viewport viewPortElement scene ( x_, y_ ) =
    let
        ratio =
            viewport.width / viewPortElement.element.width

        ( x, y ) =
            ( ratio * (x_ - viewPortElement.element.x), ratio * (y_ - viewPortElement.element.y) )

        camera =
            Scene.camera scene

        aspectRatio =
            viewport.width / viewport.height

        perspective =
            Scene.projectionMatrix aspectRatio scene

        normalizedPosition =
            ( (x * 2) / viewport.width - 1, 1 - y / viewport.height * 2 )

        homogeneousClipCoordinates =
            Vec4.vec4
                (Tuple.first normalizedPosition)
                (Tuple.second normalizedPosition)
                -1
                1

        invertedViewMatrix =
            Mat4.inverseOrthonormal (Camera.toMat4 camera)

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        vec4CameraCoordinates =
            mulVector invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4 (Vec4.getX vec4CameraCoordinates) (Vec4.getY vec4CameraCoordinates) -1 0

        vec4WorldCoordinates =
            mulVector invertedViewMatrix direction

        vec3WorldCoordinates =
            vec3 (Vec4.getX vec4WorldCoordinates) (Vec4.getY vec4WorldCoordinates) (Vec4.getZ vec4WorldCoordinates)

        normalizedVec3WorldCoordinates =
            Vec3.normalize vec3WorldCoordinates

        origin =
            Camera.position camera

        scaledDirection =
            Vec3.scale 20 normalizedVec3WorldCoordinates

        destination =
            Vec3.add origin scaledDirection
    in
    ClickPosition destination


mulVector : Mat4 -> Vec4 -> Vec4
mulVector mat v =
    let
        rec =
            Mat4.toRecord mat

        r1 =
            vec4 rec.m11 rec.m12 rec.m13 rec.m14

        r2 =
            vec4 rec.m21 rec.m22 rec.m23 rec.m24

        r3 =
            vec4 rec.m31 rec.m32 rec.m33 rec.m34

        r4 =
            vec4 rec.m41 rec.m42 rec.m43 rec.m44
    in
    vec4 (Vec4.dot r1 v) (Vec4.dot r2 v) (Vec4.dot r3 v) (Vec4.dot r4 v)
