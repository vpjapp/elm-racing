module PointTrack exposing (..)


type alias Row =
    Int


type alias Col =
    Int


type Track
    = Track (List ( Row, Col ))


getWidth : Track -> Int
getWidth (Track points) =
    points
        |> List.map Tuple.second
        |> List.maximum
        |> Maybe.withDefault 0


getHeight : Track -> Int
getHeight (Track points) =
    points
        |> List.map Tuple.first
        |> List.maximum
        |> Maybe.withDefault 0


fromString : String -> Track
fromString str =
    String.toList str
        |> stringToTupleList
        |> Track


stringToTupleList : List Char -> List ( Row, Col )
stringToTupleList list =
    case list of
        row :: col :: rest ->
            ( charToInt row, charToInt col ) :: stringToTupleList rest

        orphan :: rest ->
            [(charToInt orphan, -1)]

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
isValid (Track list) =
    List.length list
        > 3
        && ((List.filter
                (\( a, b ) -> a < 0 || b < 0)
                list
                |> List.length
            )
                == 0
           )
