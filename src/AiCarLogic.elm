module AiCarLogic exposing (..)
import LapTimer
import Model exposing (Car)
import LapTimer exposing (allNext)
import LapTimer exposing (next)
import LapTimer exposing (nextPoint)

nextPointLogic : Car -> Maybe (Float, Float)
nextPointLogic car =
    let
        lapTimer = car.lapTimer
        nextPoint = LapTimer.nextPoint lapTimer
    in
        Just nextPoint

nextTwoPointLogic : Car -> Maybe (Float, Float)
nextTwoPointLogic car =
    let
        lapTimer = car.lapTimer
        targetPoint =
            case allNext lapTimer of
                point1 :: point2 :: _ ->
                    avgPoint point1 point2

                _ ->
                    nextPoint lapTimer
    in
        Just targetPoint

avgPoint : (Float, Float) -> (Float, Float) -> (Float, Float)
avgPoint (x1, y1 ) (x2, y2) =
    ((x1 + x2) / 2, (y1 + y2) /2)