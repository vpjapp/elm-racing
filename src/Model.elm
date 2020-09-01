module Model exposing (..)

import Browser.Dom exposing (..)
import Circle2d exposing (Circle2d)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (Camera)
import Game.TwoD.Render exposing (..)
import LapTimer exposing (..)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Time exposing (Posix)
import Track exposing (Track)
import Shadow exposing (Shadow)

type alias Point = (Int, Int)

type alias RaceDetails =
    { camera : Camera
    , objects : List Renderable
    , bodies : List BodySpec
    , resources : Resources
    , forces : List ( String, Vector )
    , debug : List String
    , toggler : Bool
    , dimensions : ( Int, Int )
    , track : Track
    , cars : List Car
    , raceState: RaceState
    , updateInProgress: Bool
    , skippedFrames: Int
    }

type RaceState
    = Starting Int
    | Racing
    | Paused
    | Finished

type alias Car =
    { body : BodySpec
    , targetPoint : Maybe ( Float, Float )
    , onTrack : Bool
    , carControl : CarControl
    , lapTimer : LapTimer
    , shadow: Shadow
    }


type CarControl
    = CursorToSelf
    | CursorToPoint
        { point : Point2d Meters Length
        , circle : Circle2d Meters Length
        }
    | AiControl (Car -> Maybe (Float, Float))


type Model
    = Loading { resources : Resources, dimensions : Maybe ( Int, Int ) }
    | Menu { resources : Resources, dimensions : ( Int, Int ) }
    | LoadingTrack { resources : Resources, dimensions : ( Int, Int ) }
    | Race RaceDetails


type alias BodySpec =
    { x : Float
    , y : Float
    , width : Int
    , height : Int

    -- Angle in radians
    , rotation : Float
    , mass : Int
    , id : String
    , velocity : Vector
    , type_ : String
    }


type alias Vector =
    { x : Float, y : Float }


type Msg
    = NoOp
    | Resources Resources.Msg
    | ResFail String
    | UpdatePhysics (List BodySpec)
    | GenerateTrackAndCars Int
    | StartGeneratingTrackAndCars Int
    | StepTime
    | SetTargetPoint String (Maybe ( Float, Float ))
    | StepAnimation Float
    | StartUpdateLoop Float
    | SetScreenSize Viewport
    | StartTimer Posix
    | UpdateTargetPoints
    | CountDown Int
    | TogglePause



type TrackTile
    = Empty
    | Vertical
    | Horizontal
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft
    | Cross
