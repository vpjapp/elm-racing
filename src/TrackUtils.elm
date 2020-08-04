module TrackUtils exposing (..)

import Array exposing (fromList, get)
import Point2d exposing (Point2d)
import Length exposing (Length)
import Quantity exposing (Quantity)
import Color exposing (Color)
import Game.TwoD.Render exposing (shapeWithOptions)
import Game.TwoD.Render exposing (circle)
import Game.TwoD.Render exposing (rectangle)

tileCenter : Float -> Float -> Float
tileCenter index size =
    index * size + (size / 2)

f = toFloat

tileCenterPoint : ( Int, Int ) -> Int -> ( Float, Float )
tileCenterPoint indexPoint size =
    ( tileCenter (f (Tuple.second indexPoint)) (f size), tileCenter (f (Tuple.first indexPoint)) (f size) )

getCoordinatesForIndex : Int -> Int -> Int -> (Float, Float)
getCoordinatesForIndex row col gridWidth =
    ( tileCenter (f col) (f gridWidth), tileCenter (f row) (f gridWidth) )

getPairs : ( a, a ) -> List ( a, a ) -> List ( ( a, a ), ( a, a ) )
getPairs default points =
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
                            ( point, Array.get 0 pointArray |> Maybe.withDefault default )
            in
            pair
        )
        points


pointToTuple: Point2d Length.Meters Length -> (Float, Float)
pointToTuple point =
    Point2d.toTuple Length.inMeters point

pointToIntTuple: Point2d Length.Meters Length  -> (Int, Int)
pointToIntTuple point =
    pointToTuple point
        |> Tuple.mapBoth round round

tupleToFloatTuple: (Length , Length) -> (Float, Float)
tupleToFloatTuple tuple  =
    tuple |> Tuple.mapBoth Length.inMeters Length.inMeters

tupleToPoint : (Float, Float) -> Point2d Length.Meters Length
tupleToPoint (x, y) =
    Point2d.xy (Length.meters x) (Length.meters y)

debugSpots ( width, height ) =
    [ debugSpot Color.brown ( 0, 0 )
    , debugSpot Color.orange ( f width, 0 )
    , debugSpot Color.blue ( f width, f height )
    , debugSpot Color.yellow ( 0, f height )
    ]

debugSpot color ( x, y ) size =
    shapeWithOptions
        circle
        { color = color
        , position = ( x, y, 0 )
        , size = ( size, size )
        , pivot = ( 0.5, 0.5 )
        , rotation = 0.0
        }

drawRect color ( x, y ) size rotation =
    shapeWithOptions
        rectangle
        { color = color
        , position = ( x, y, 0 )
        , size = size
        , pivot = ( 0.5, 0.5 )
        , rotation = rotation
        }

intPointToFloatPoint : (Int, Int) -> (Float, Float)
intPointToFloatPoint = Tuple.mapBoth f f