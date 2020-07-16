module LapTimer exposing (..)

import Circle2d exposing (Circle2d)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)


type TrackSequencer
    = TrackSequencer TrackSequencerData


type alias TrackSequencerData =
    { past : List TrackSequence
    , next : TrackSequence
    , upcoming : List TrackSequence
    , lapNumber : Int
    }


type TrackSequence
    = Circle (Circle2d Length.Meters Length)
    | Rectangle (Rectangle2d Length.Meters Length)


next : TrackSequencer -> TrackSequence
next (TrackSequencer data) =
    data.next


getLapNumber : TrackSequencer -> Int
getLapNumber (TrackSequencer data) =
    data.lapNumber


update : TrackSequencer -> ( Float, Float ) -> TrackSequencer
update (TrackSequencer data) ( x, y ) =
    let
        contains =
            case data.next of
                Circle circle ->
                    Circle2d.contains (Point2d.xy (Length.meters x) (Length.meters y)) circle

                Rectangle rectangle ->
                    Rectangle2d.contains (Point2d.xy (Length.meters x) (Length.meters y)) rectangle
    in
    if contains then
        TrackSequencer (moveToNext data)

    else
        TrackSequencer data


moveToNext : TrackSequencerData -> TrackSequencerData
moveToNext data =
    case data.upcoming of
        [] ->
            case data.past of
                [] ->
                    data

                --This should be in possible
                first :: rest ->
                    { past = []
                    , next = first
                    , upcoming = rest
                    , lapNumber = data.lapNumber + 1
                    }

        toNext :: rest ->
            { past = data.past ++ [ data.next ]
            , next = toNext
            , upcoming = rest
            , lapNumber = data.lapNumber
            }
