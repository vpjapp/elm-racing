module Model exposing (..)

import Browser.Dom exposing (..)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (Camera)
import Game.TwoD.Render exposing (..)
import Track exposing (Track)


type alias RaceDetails =
    { camera : Camera
    , objects : List Renderable
    , bodies : List BodySpec
    , resources : Resources
    , forces : List Vector
    , debug : List String
    , toggler : Bool
    , dimensions : ( Int, Int )
    , track : Track
    , car : Car
    }


type alias Car =
    { body : BodySpec
    , targetPoint : Maybe ( Float, Float )
    , onTrack : Bool
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
    | AddBodies
    | StepTime
    | SetTargetPoint (Maybe ( Float, Float ))
    | StepAnimation Float
    | SetScreenSize Viewport


type TrackTile
    = Empty
    | Vertical
    | Horizontal
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft
    | Cross
