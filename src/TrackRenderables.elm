module TrackRenderables exposing (..)

import Array exposing (fromList, get)
import Color
import Game.TwoD.Render exposing (Renderable, circle, shapeWithOptions)
import Game.TwoD.Render exposing (rectangle)


getRenderables : Int -> Int -> List ( Int, Int ) -> List Renderable
getRenderables gridWidth trackWidth points =
    List.map
        (getCircle
            gridWidth
            trackWidth
        )
        points
        ++ getStraights gridWidth trackWidth points


getCircle : Int -> Int -> ( Int, Int ) -> Renderable
getCircle gridWidth trackWidth ( row, col ) =
    let
        floatCol =
            toFloat col

        floatRow =
            toFloat row

        floatGridWidth =
            toFloat gridWidth

        ( x, y ) =
            ( tileCenter floatCol floatGridWidth, tileCenter floatRow floatGridWidth )

        size =
            trackWidth
    in
    shapeWithOptions circle
        { color = Color.gray
        , position = ( x, y, 0 )
        , size = ( toFloat size, toFloat size )
        , rotation = 0
        , pivot = ( 0.5, 0.5 )
        }


tileCenter : Float -> Float -> Float
tileCenter index size =
    index * size + (size / 2)


tileCenterPoint : ( Int, Int ) -> Int -> ( Float, Float )
tileCenterPoint indexPoint size =
    ( tileCenter (f (Tuple.second indexPoint)) (f size), tileCenter (f (Tuple.first indexPoint)) (f size) )


getStraights : Int -> Int -> List ( Int, Int ) -> List Renderable
getStraights gridWidth trackWidth points =
    let
        pairs =
            getPairs points
            |> Debug.log "Pairs"
    in
    List.map
        (pairToStraight gridWidth trackWidth)
        pairs


getPairs : List ( Int, Int ) -> List ( ( Int, Int ), ( Int, Int ) )
getPairs points =
    List.indexedMap
        (\index point ->
            let
                pointArray =
                    Array.fromList points

                point2 =
                    Array.get (index + 1) pointArray

                pair =
                    case point2 of
                        Just p2 ->
                            ( point, p2 )

                        Nothing ->
                            ( point, Array.get 0 pointArray |> Maybe.withDefault ( 0, 0 ) )
            in
            pair
        )
        points


pairToStraight : Int -> Int -> ( ( Int, Int ), ( Int, Int ) ) -> Renderable
pairToStraight gridSize trackWidth ( start, end ) =
    let
        _ = Debug.log "#####################" "asd"
        startTileCenter =
            tileCenterPoint start gridSize
            |> Debug.log "startTileCenter"

        endTileCenter =
            tileCenterPoint end gridSize
            |> Debug.log "endTileCenter"

        (centerPointX, centerPointY) = pairAvg startTileCenter endTileCenter
            |> Debug.log "centerpoint"

        rowDiff = abs (Tuple.first start - Tuple.first end)
            |> Debug.log "rowDiff"
        colDiff = abs (Tuple.second start - Tuple.second end)
            |> Debug.log "colDiff"

        width = (if rowDiff == 0 then colDiff * gridSize else trackWidth)
            |> Debug.log "Width"

        height = (if rowDiff /= 0 then rowDiff * gridSize else trackWidth)
            |> Debug.log "Height"

        aa = Debug.log "END #####################" "asd"
        -- Figure out if horizontal or vertical
        -- Calculate distance between points
        -- Set width and height for the square accorging to track size and distance between points
    in
    shapeWithOptions
        rectangle
        { color = Color.gray
        , position = ( centerPointX, centerPointY, 0 )
        , size = ( f width, f height )
        , pivot = (0.5, 0.5 )
        , rotation = 0
        }


f =
    toFloat

pairAvg (x1, y1) (x2, y2) =
    ((x1 + x2) / 2, (y1 + y2) / 2)