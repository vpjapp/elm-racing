module Model exposing (..)

import Browser.Dom exposing (..)
import Circle2d exposing (Circle2d)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (Camera)
import Game.TwoD.Render exposing (..)
import LapTimer exposing (..)
import Length exposing (Length, Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Time exposing (Posix)
import Track exposing (Track)


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
    }


type alias Car =
    { body : BodySpec
    , targetPoint : Maybe ( Float, Float )
    , onTrack : Bool
    , carControl : CarControlPoint
    , lapTimer : LapTimer
    }


type CarControlPoint
    = Self
    | Point
        { point : Point2d Meters Length
        , circle : Circle2d Meters Length
        }


type Model
    = Loading { resources : Resources, dimensions : Maybe ( Int, Int ) }
    | Menu { resources : Resources, dimensions : ( Int, Int ) }
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
    | AddBodies String
    | StepTime
    | SetTargetPoint String (Maybe ( Float, Float ))
    | StepAnimation Float
    | SetScreenSize Viewport
    | StartTimer Posix


type TrackTile
    = Empty
    | Vertical
    | Horizontal
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft
    | Cross
