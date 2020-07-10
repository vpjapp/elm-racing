module Track exposing (..)

import Circle2d exposing (Circle2d)
import Game.TwoD.Render exposing (Renderable)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import TrackGeometry exposing (getCircles, getRectangles)
import TrackRenderables exposing (getRenderables)


type alias Row =
    Int


type alias Col =
    Int


type Track
    = Track
        { points : List ( Row, Col )
        , renderables : List Renderable
        , circle2ds : List (Circle2d Length.Meters Length)
        , rectangle2ds : List (Rectangle2d Length.Meters Length)
        }


getWidth : Track -> Int
getWidth (Track { points }) =
    points
        |> List.map Tuple.second
        |> List.maximum
        |> Maybe.withDefault -1
        |> (+) 1


getHeight : Track -> Int
getHeight (Track { points }) =
    points
        |> List.map Tuple.first
        |> List.maximum
        |> Maybe.withDefault -1
        |> (+) 1


fromString : Int -> Int -> String -> Track
fromString gridWidth trackWidth str =
    str
        |> String.replace " " ""
        |> String.toList
        |> stringToTupleList
        |> fromTuples gridWidth trackWidth


fromTuples : Int -> Int -> List ( Row, Col ) -> Track
fromTuples gridWidth trackWidth points =
    Track
        { points = points
        , renderables = getRenderables gridWidth trackWidth points
        , circle2ds = getCircles gridWidth trackWidth points
        , rectangle2ds = getRectangles gridWidth trackWidth points
        }


stringToTupleList : List Char -> List ( Row, Col )
stringToTupleList list =
    case list of
        row :: col :: rest ->
            ( charToInt row, charToInt col ) :: stringToTupleList rest

        orphan :: rest ->
            [ ( charToInt orphan, -1 ) ]

        [] ->
            []


charToInt : Char -> Int
charToInt char =
    let
        intChar =
            String.fromChar char |> String.toInt
    in
    case intChar of
        Just int ->
            int

        Nothing ->
            case Char.toUpper char of
                'A' ->
                    0

                'B' ->
                    1

                'C' ->
                    2

                'D' ->
                    3

                'E' ->
                    4

                'F' ->
                    5

                'G' ->
                    6

                'H' ->
                    7

                'I' ->
                    8

                'J' ->
                    9

                _ ->
                    -1


isValid : Track -> Bool
isValid (Track { points }) =
    List.length points
        > 3
        && ((List.filter
                (\( a, b ) -> a < 0 || b < 0)
                points
                |> List.length
            )
                == 0
           )


toRenderables : Track -> List Renderable
toRenderables (Track { renderables }) =
    renderables


startPoint : Track -> ( Int, Int )
startPoint (Track track) =
    track.points |> List.head |> Maybe.withDefault ( 100, 100 )


onTrack : Track -> ( Float, Float ) -> Bool
onTrack (Track track) ( x, y ) =
    List.any
        (\rect ->
            Rectangle2d.contains (point ( x, y )) rect
        )
        track.rectangle2ds
        || List.any
            (\circle -> Circle2d.contains (point ( x, y )) circle)
            track.circle2ds


point : ( Float, Float ) -> Point2d Length.Meters Length
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


f =
    toFloat

toCircles : Track -> List (Circle2d Length.Meters Length)
toCircles (Track track) =
    track.circle2ds

toRectangles : Track -> List (Rectangle2d Length.Meters Length)
toRectangles (Track track) =
    track.rectangle2ds