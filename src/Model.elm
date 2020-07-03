module Model exposing (..)

import Browser.Dom exposing (..)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (Camera)
import Game.TwoD.Render exposing (..)
import Track exposing (Track)


type alias Model =
    { camera : Camera
    , objects : List Renderable
    , bodies : List BodySpec
    , resources : Resources
    , targetPoint : Maybe ( Float, Float )
    , forces : List Vector
    , debug : List String
    , toggler : Bool
    , width : Int
    , height : Int
    , track : Track
    }


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
