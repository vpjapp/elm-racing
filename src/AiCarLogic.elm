module AiCarLogic exposing (..)

import LapTimer exposing (allNext, next, nextPoint)
import Model exposing (Car)


nextPointLogic : Car -> Maybe ( Float, Float )
nextPointLogic car =
    let
        lapTimer =
            car.lapTimer

        nextPoint =
            LapTimer.nextPoint lapTimer
    in
    Just nextPoint


nextTwoPointLogic : Car -> Maybe ( Float, Float )
nextTwoPointLogic car =
    let
        lapTimer =
            car.lapTimer

        targetPoint =
            case allNext lapTimer of
                point1 :: point2 :: _ ->
                    avgPoint point1 point2

                _ ->
                    nextPoint lapTimer
    in
    Just targetPoint


nextTwoPointLogicWithBraking : Car -> Maybe ( Float, Float )
nextTwoPointLogicWithBraking car =
    let
        lapTimer =
            car.lapTimer

        targetPoint =
            case allNext lapTimer of
                point1 :: point2 :: _ ->
                    avgPoint point1 point2

                _ ->
                    nextPoint lapTimer

        carPoint =
            ( car.body.x, car.body.y )
    in
    if manhattanDistance carPoint targetPoint < 2500 then
        Nothing

    else
        Just targetPoint


avgPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
avgPoint ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2, (y1 + y2) / 2 )


avg3Point : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
avg3Point ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) =
    ( (x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3 )


manhattanDistance : ( Float, Float ) -> ( Float, Float ) -> Float
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    let
        ( dx, dy ) =
            ( abs <| x1 - x2, abs <| y1 - y2 )
    in
    dx + dy
