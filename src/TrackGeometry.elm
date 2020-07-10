module TrackGeometry exposing (..)

import Angle exposing (Angle)
import Circle2d exposing (Circle2d)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import TrackRenderables exposing (pairAvg)
import TrackUtils exposing (f, getCoordinatesForIndex, getPairs, tileCenterPoint)


getCircles : Int -> Int -> List ( Int, Int ) -> List (Circle2d Length.Meters Length)
getCircles gridWidth trackWidth points =
    List.map
        (\( x, y ) ->
            let
                ( centerX, centerY ) =
                    getCoordinatesForIndex x y gridWidth

                centerPoint =
                    Point2d.meters centerX centerY
            in
            Circle2d.withRadius (Length.meters <| f trackWidth / 2) centerPoint
        )
        points


getRectangles : Int -> Int -> List ( Int, Int ) -> List (Rectangle2d Length.Meters Length)
getRectangles gridWidth trackWidth points =
    let
        pairs =
            getPairs ( 0, 0 ) points
    in
    List.map
        (pairToRectangle gridWidth trackWidth)
        pairs



-- getRectangles gridWidth trackWidth points =
--     List.map
--         (\( x, y ) ->
--             let
--                 ( centerX, centerY ) =
--                     getCoordinatesForIndex x y gridWidth
--                 centerPoint =
--                     Point2d.meters centerX centerY
--             in
--             Rectangle2d.withDimensions ( trackWidth |> m, trackWidth |> m ) (Angle.degrees 0) centerPoint
--         )
--         points


pairToRectangle : Int -> Int -> ( ( Int, Int ), ( Int, Int ) ) -> Rectangle2d Length.Meters Length
pairToRectangle gridSize trackWidth ( start, end ) =
    let
        startTileCenter =
            tileCenterPoint start gridSize

        endTileCenter =
            tileCenterPoint end gridSize

        ( centerPointX, centerPointY ) =
            pairAvg startTileCenter endTileCenter

        rowDiff =
            abs (Tuple.first start - Tuple.first end)

        colDiff =
            abs (Tuple.second start - Tuple.second end)

        width =
            if rowDiff == 0 then
                colDiff * gridSize

            else
                trackWidth

        height =
            if rowDiff /= 0 then
                rowDiff * gridSize

            else
                trackWidth

        -- Figure out if horizontal or vertical
        -- Calculate distance between points
        -- Set width and height for the square accorging to track size and distance between points
    in
    Rectangle2d.withDimensions ( width |> m, height |> m )
        (Angle.degrees 0)
        (Point2d.xy (centerPointX |> fm)
            (centerPointY |> fm)
        )



-- let
--     (startX, startY) = start
--     (endX, endY) = end
--     width = abs ((endX - startX ) * gridSize)
--     height = abs ((endY - startY) * gridSize)
--     centerPoint =
--         Point2d.meters centerX centerY
-- in
-- Rectangle2d.withDimensions ( trackWidth |> m, trackWidth |> m ) (Angle.degrees 0) centerPoint


m x =
    x |> f |> Length.meters

fm x =
    x |> Length.meters
