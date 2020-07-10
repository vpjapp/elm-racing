module TrackUtils exposing (..)

import Array exposing (fromList, get)
import Point2d exposing (Point2d)
import Length exposing (Length)
import Quantity exposing (Quantity)

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