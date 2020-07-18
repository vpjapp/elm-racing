module LapTimer exposing (..)

import Circle2d exposing (Circle2d)
import Length exposing (Length)
import List.Extra exposing (interweave)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Time exposing (Posix)
import TrackUtils exposing (tupleToPoint)


type LapTimer
    = LapTimer LapTimerData


type alias LapTimerData =
    { past : List TrackSequence
    , next : TrackSequence
    , upcoming : List TrackSequence
    , lapNumber : Int
    , offset : Float
    }


type TrackSequence
    = Circle (Circle2d Length.Meters Length)
    | Rectangle (Rectangle2d Length.Meters Length)


dummyCircle : Circle2d Length.Meters Length
dummyCircle =
    Circle2d.withRadius (Length.meters 10) (tupleToPoint ( 0, 0 ))


next : LapTimer -> TrackSequence
next (LapTimer data) =
    data.next


nextPoint : LapTimer -> ( Float, Float )
nextPoint (LapTimer data) =
    case data.next of
        Rectangle rect ->
            Rectangle2d.centerPoint rect
                |> TrackUtils.pointToTuple

        Circle circ ->
            Circle2d.centerPoint circ
                |> TrackUtils.pointToTuple


getLapNumber : LapTimer -> Int
getLapNumber (LapTimer data) =
    data.lapNumber


update : LapTimer -> ( Float, Float ) -> Float -> LapTimer
update (LapTimer data) ( x, y ) deltaTime =
    let
        contains =
            case data.next of
                Circle circle ->
                    Circle2d.contains (Point2d.xy (Length.meters x) (Length.meters y)) circle

                Rectangle rectangle ->
                    Rectangle2d.contains (Point2d.xy (Length.meters x) (Length.meters y)) rectangle
    in
    if contains then
        LapTimer (moveToNext data deltaTime)

    else
        LapTimer { data | offset = data.offset + deltaTime }

-- BUG HERE
-- The lap should change one step further on. Now it changes when the next -point to the first rectangle
-- it should change only when the first rectangle is reached.
moveToNext : LapTimerData -> Float -> LapTimerData
moveToNext data deltaTime =
    case data.upcoming of
        [] ->
            case data.past of
                --This should be impossible
                [] ->
                    data

                -- Lap full
                first :: rest ->
                    { past = []
                    , next = first
                    , upcoming = rest ++ [ data.next ]
                    , lapNumber = data.lapNumber + 1
                    , offset = 0
                    }

        toNext :: rest ->
            { past = data.past ++ [ data.next ]
            , next = toNext
            , upcoming = rest
            , lapNumber = data.lapNumber
            , offset = data.offset + deltaTime
            }


addOffset : LapTimer -> Float -> LapTimer
addOffset (LapTimer data) delta =
    LapTimer { data | offset = data.offset + delta }


timer : {- List (Circle2d Length.Meters Length) -> -} List (Rectangle2d Length.Meters Length) -> LapTimer
timer {- circles -} rectangles =
    case rectangles of
        firstRectangles :: restRectangles ->
            LapTimer
                { past = []
                , next = Rectangle firstRectangles
                , upcoming = List.map Rectangle restRectangles
                , lapNumber = 1
                , offset = 0
                }

        [] ->
            LapTimer
                { past = []
                , next = Circle dummyCircle
                , upcoming = []
                , lapNumber = 0
                , offset = 0
                }


text : LapTimer -> String
text (LapTimer data) =
    "Lap " ++ String.fromInt data.lapNumber ++ ": " ++ String.fromFloat (data.offset / 1000)
