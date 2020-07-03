module Track exposing (..)

import Game.TwoD.Render exposing (Renderable)
import TrackRenderables exposing (getRenderables)


type alias Row =
    Int


type alias Col =
    Int


type Track
    = Track
        { points : List ( Row, Col )
        , renderables : List Renderable
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
    String.toList str
        |> stringToTupleList
        |> fromTuples gridWidth trackWidth


fromTuples : Int -> Int -> List ( Row, Col ) -> Track
fromTuples gridWidth trackWidth points =
    Track { points = points, renderables = getRenderables gridWidth trackWidth points }


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
