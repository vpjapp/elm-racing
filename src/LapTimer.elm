module LapTimer exposing (..)

import Circle2d exposing (Circle2d)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
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
    , fastestLap : ( Int, Float )
    , lastLapTime : Float
    }


type TrackSequence
    = Circle (Circle2d Length.Meters Length)
    | Rectangle (Rectangle2d Length.Meters Length)

isBestLap : LapTimer -> Bool
isBestLap (LapTimer lapTimer) =
    let
        (_, bestTime) = lapTimer.fastestLap
    in
    lapTimer.lastLapTime == bestTime

differentLaps: LapTimer -> LapTimer -> Bool
differentLaps (LapTimer lapTimer1) (LapTimer lapTimer2) =
    lapTimer1.lapNumber /= lapTimer2.lapNumber

getTime : LapTimer -> Float
getTime (LapTimer data) =
    data.offset / 1000

dummyCircle : Circle2d Length.Meters Length
dummyCircle =
    Circle2d.withRadius (Length.meters 10) (tupleToPoint ( 0, 0 ))


next : LapTimer -> TrackSequence
next (LapTimer data) =
    data.next


allNext : LapTimer -> List ( Float, Float )
allNext (LapTimer data) =
    data.next
        :: data.upcoming
        ++ data.past
        |> List.map
            sequenceToPoint


nextPoint : LapTimer -> ( Float, Float )
nextPoint (LapTimer data) =
    sequenceToPoint data.next


sequenceToPoint : TrackSequence -> ( Float, Float )
sequenceToPoint seq =
    case seq of
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

                -- Revolve lists
                first :: rest ->
                    { past = []
                    , next = first
                    , upcoming = rest ++ [ data.next ]
                    , lapNumber = data.lapNumber
                    , offset = data.offset + deltaTime
                    , fastestLap = data.fastestLap
                    , lastLapTime = data.lastLapTime
                    }

        toNext :: rest ->
            if List.length data.past == 0 then
                -- Next lap
                let
                    currentLapTime =
                        data.offset + deltaTime

                    ( fLap, fTime ) =
                        data.fastestLap

                    fastestLap =
                        if fLap == 0 || currentLapTime < fTime then
                            ( data.lapNumber, currentLapTime )

                        else
                            data.fastestLap
                in
                { past = data.past ++ [ data.next ]
                , next = toNext
                , upcoming = rest
                , lapNumber = data.lapNumber + 1
                , offset = 0
                , fastestLap = fastestLap
                , lastLapTime = currentLapTime
                }

            else
                { past = data.past ++ [ data.next ]
                , next = toNext
                , upcoming = rest
                , lapNumber = data.lapNumber
                , offset = data.offset + deltaTime
                , fastestLap = data.fastestLap
                , lastLapTime = data.lastLapTime
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
                , lapNumber = 0
                , offset = 0
                , fastestLap = ( 0, 0 )
                , lastLapTime = 0
                }

        [] ->
            LapTimer
                { past = []
                , next = Circle dummyCircle
                , upcoming = []
                , lapNumber = 0
                , offset = 0
                , fastestLap = ( 0, 0 )
                , lastLapTime = 0
                }


render : LapTimer -> List (Html msg)
render (LapTimer data) =
    [ Html.span [ class "lap" ] [ Html.text ("Lap " ++ String.fromInt data.lapNumber ++ ": " ++ String.fromFloat (data.offset / 1000)) ]
    , Html.span [ class "lap" ] <|
        [ Html.text <|
            "Fastest: "
                ++ (String.fromInt <| fastestLapNum (LapTimer data))
                ++ " / "
                ++ (String.fromFloat <| fastestLapTime (LapTimer data) / 1000)
        ]
    , Html.span [ class "lap" ] <|
        [ Html.text <|
            "Last: "
                ++ (String.fromFloat <| data.lastLapTime / 1000)
        ]
    ]


fastestLapNum : LapTimer -> Int
fastestLapNum (LapTimer data) =
    Tuple.first data.fastestLap


fastestLapTime : LapTimer -> Float
fastestLapTime (LapTimer data) =
    Tuple.second data.fastestLap
