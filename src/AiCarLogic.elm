module AiCarLogic exposing (..)
import LapTimer
import Model exposing (Car)

nextPointLogic : Car -> Maybe (Float, Float)
nextPointLogic car =
    let
        lapTimer = car.lapTimer
        nextPoint = LapTimer.nextPoint lapTimer
    in
        Just nextPoint