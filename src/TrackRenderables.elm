module TrackRenderables exposing (..)

import Color
import Game.TwoD.Render exposing (Renderable, circle, rectangle, shapeWithOptions)
import TrackUtils exposing (debugSpot, getCoordinatesForIndex, getPairs, intPointToFloatPoint, tileCenterPoint, tupleToFloatTuple)
import TrackUtils exposing (drawRect)


getRenderables : Int -> Int -> List ( Int, Int ) -> List Renderable
getRenderables gridWidth trackWidth points =
    List.map
        (getCircle
            gridWidth
            trackWidth
        )
        points
        ++ getStraights gridWidth trackWidth points
        ++ getStartLine (List.take 2 points) gridWidth trackWidth


getCircle : Int -> Int -> ( Int, Int ) -> Renderable
getCircle gridWidth trackWidth ( row, col ) =
    let
        ( x, y ) =
            getCoordinatesForIndex row col gridWidth

        size =
            trackWidth
    in
    shapeWithOptions circle
        { color = Color.darkGray
        , position = ( x, y, 0 )
        , size = ( toFloat size, toFloat size )
        , rotation = 0
        , pivot = ( 0.5, 0.5 )
        }


getStraights : Int -> Int -> List ( Int, Int ) -> List Renderable
getStraights gridWidth trackWidth points =
    let
        pairs =
            getPairs ( 0, 0 ) points
    in
    List.map
        (pairToStraight gridWidth trackWidth)
        pairs


pairToStraight : Int -> Int -> ( ( Int, Int ), ( Int, Int ) ) -> Renderable
pairToStraight gridSize trackWidth ( start, end ) =
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
    shapeWithOptions
        rectangle
        { color = Color.darkGray
        , position = ( centerPointX, centerPointY, 0 )
        , size = ( f width, f height )
        , pivot = ( 0.5, 0.5 )
        , rotation = 0
        }


f =
    toFloat


pairAvg ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2, (y1 + y2) / 2 )


getStartLine : List ( Int, Int ) -> Int -> Int -> List Renderable
getStartLine points gridSize trackSize =
    case points of
        ( x1, y1 ) :: ( x2, y2 ) :: _ ->
            let
                vertical = x1 - x2 == 0

                size =
                        intPointToFloatPoint (100, trackSize)
                rotation =
                    if vertical then
                        0
                    else
                        pi/2


                startPoint =
                    ( f (y1 * gridSize) + 0.5 * f gridSize, f (x1 * gridSize) + 0.5 * f gridSize )
                secondPoint =
                    ( f (y2 * gridSize) + 0.5 * f gridSize, f (x2 * gridSize) + 0.5 * f gridSize )
                -- _ = Debug.log "Start points" points
                -- a1 = Debug.log "Start point" (intPointToFloatPoint startPoint)
            in
            [ drawRect Color.lightGray startPoint size rotation
            -- , debugSpot Color.darkGreen secondPoint (f trackSize)
            ]

        _ ->
            []
