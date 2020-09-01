module Main exposing (..)

import Angle
import Browser exposing (Document)
import Browser.Dom exposing (..)
import Browser.Events exposing (onAnimationFrameDelta)
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Direction2d
import Game.Resources as Resources exposing (Resources)
import Game.TwoD exposing (..)
import Game.TwoD.Camera exposing (fixedArea)
import Game.TwoD.Render exposing (..)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import LapTimer exposing (LapTimer)
import Length exposing (Length)
import Model exposing (..)
import Platform.Sub exposing (batch)
import Point2d exposing (Point2d)
import Ports exposing (..)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Shadow exposing (Shadow)
import Task
import Time
import Track exposing (..)
import TrackUtils exposing (debugSpot, pointToTuple, tupleToFloatTuple)
import Update exposing (update)
import Vector2d



-- TODO state transfer from loading -> menu and menu button clicking and setting up the race after click.
---- MODEL ----
--   0 1 2 3 4 5 6 7
-- F
-- E
-- D
-- C
-- B
-- A


tracks =
    [ "E0 E2 D2 D4 B4 B2 A2 A1 C1 C0"
    , "E0 E8 A8 A0"
    , "E0 E3 A3 A6 C6 C0"
    , "F0 F1 B1 B2 F2 F3 B3 B4 F4 F5 A5 A0"
    , "E0 E2 D2 D4 F4 F3 C3 C1 A1 A3 B3 B0"
    ]


trackDimensions =
    ( 4, 3 )


init : ( Model, Cmd Msg )
init =
    ( Loading { resources = Resources.init, dimensions = Nothing }
      --     { camera = fixedArea (1200 * 800) ( 600, 400 )
      --   , objects = []
      --   , bodies = [] --initialBodies
      --   , resources = Resources.init
      --   , targetPoint = Nothing
      --   , forces = []
      --   , debug = []
      --   , toggler = False
      --   , width = 1200
      --   , height = 800
      --   , track = Track.fromString 0 0 "A0"
      --   }
    , Cmd.batch
        [ Resources.loadTextures [ "./car.png" ]
            |> Cmd.map Resources
        , getViewport |> Task.perform SetScreenSize
        ]
    )



---- VIEW ----


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


view : Model -> Document Msg
view model =
    { title = "Elm Racing"
    , body =
        bodyView model
    }


bodyView model =
    case model of
        Race mdl ->
            [ renderCenteredWithOptions
                []
                [ Pointer.onDown (relativePos >> Just >> SetTargetPoint "car-1")
                , Touch.onMove (touchCoordinates >> Just >> SetTargetPoint "car-1")
                , Pointer.onMove (relativePos >> Just >> SetTargetPoint "car-1")
                , Pointer.onUp (\_ -> SetTargetPoint "car-1" Nothing)
                , Pointer.onOut (\_ -> SetTargetPoint "car-1" Nothing)
                , Pointer.onLeave (\_ -> SetTargetPoint "car-1" Nothing)
                ]
                { time = 0
                , size = mdl.dimensions
                , camera = mdl.camera
                }
                (toRenderables mdl.track
                    ++ mdl.objects
                    ++ debugLapTimer mdl.cars
                    ++ debugForces mdl.cars mdl.forces
                    -- ++ debugSpots mdl.dimensions
                    ++ debugTargetPoint mdl.cars
                    -- ++ debugTrack mdl.track 2000
                    ++ debugOnTrack mdl.cars
                    ++ bodySpecsToRenderables mdl.resources mdl.bodies
                    ++ renderControlPoint mdl.cars
                    ++ renderShadow mdl.cars
                )

            -- , button [ onClick StepTime ] [ text "StepTime" ]
            -- , div [] (renderDebug model)
            , div [ class "lap-timer" ]
                (case mdl.cars |> List.head of
                    Just car ->
                        LapTimer.render car.lapTimer

                    Nothing ->
                        []
                )
            , button [class "pause-button", onClick TogglePause ] [ text "Pause" ]
            , renderCountDown mdl.raceState
            ]

        Menu mdl ->
            h1 [] [ text "Choose track" ]
                :: List.map
                    (\trackNro ->
                        div [ class "track-chooser-button", onClick <| StartGeneratingTrackAndCars trackNro ] []
                    )
                    (List.range
                        0
                        2000
                    )

        Loading mdl ->
            [ h1 [] [ text "Loading... Please wait." ] ]

        LoadingTrack mdl ->
            [ h1 [] [ text "Generating track... Please wait." ] ]


getTileSize : Int -> Int -> Int
getTileSize pixels tileSize =
    pixels // tileSize


renderCountDown : RaceState -> Html msg
renderCountDown raceState =
    case raceState of
        Starting number ->
            h1 [ class "count-down" ] [ text <| String.fromInt number ]

        _ ->
            text ""


renderControlPoint : List Car -> List Renderable
renderControlPoint cars =
    List.filterMap
        (\car ->
            case car.carControl of
                CursorToSelf ->
                    Nothing

                AiControl _ ->
                    Nothing

                CursorToPoint control ->
                    let
                        ( x, y ) =
                            pointToTuple control.point

                        radius =
                            Circle2d.radius control.circle |> Length.inMeters
                    in
                    Just <|
                        shapeWithOptions circle
                            { color = Color.green
                            , position = ( x, y, 0 )
                            , size = ( radius, radius )
                            , rotation = 0
                            , pivot = ( 0.5, 0.5 )
                            }
        )
        cars


debugOnTrack : List Car -> List Renderable
debugOnTrack cars =
    List.map
        (\car ->
            debugSpot
                (if car.onTrack then
                    Color.green

                 else
                    Color.red
                )
                ( car.body.x, car.body.y )
                400
        )
        cars


renderDebug : RaceDetails -> List (Html Msg)
renderDebug model =
    div [] [ text "Debugs:" ]
        :: List.map
            (\d ->
                div [] [ text d ]
            )
            model.debug


getCar : RaceDetails -> Maybe BodySpec
getCar model =
    case model.bodies of
        body :: _ ->
            Just body

        _ ->
            Nothing


debugTargetPoint : List Car -> List Renderable
debugTargetPoint cars =
    List.filterMap
        (\car ->
            case car.targetPoint of
                Nothing ->
                    Nothing

                Just ( x, y ) ->
                    Just <| debugSpot Color.darkRed ( x, y ) 100
        )
        cars


f =
    toFloat


debugForces : List Car -> List ( String, Vector ) -> List Renderable
debugForces cars forces =
    List.filterMap
        (\car ->
            let
                carForce =
                    List.filter (\( forceId, _ ) -> forceId == car.body.id) forces
                        |> List.head
            in
            case carForce of
                Nothing ->
                    Nothing

                Just ( _, { x, y } ) ->
                    let
                        vector =
                            Vector2d.unitless x y

                        dir =
                            Vector2d.direction vector |> Maybe.withDefault Direction2d.positiveY

                        angle =
                            Direction2d.toAngle dir

                        radians =
                            Angle.inRadians angle - (pi / 2)
                    in
                    Just <|
                        shapeWithOptions
                            rectangle
                            { color = Color.red
                            , position = ( car.body.x, car.body.y, 0 )
                            , size = ( 20, 400 )
                            , pivot = ( 1, 0 )
                            , rotation = radians
                            }
        )
        cars



-- case mBodySpec of
--     Nothing ->
--         []
--     Just car ->
--         List.map
--             (\{ x, y } ->
--                 let
--                     vector =
--                         Vector2d.unitless x y
--                     dir =
--                         Vector2d.direction vector |> Maybe.withDefault Direction2d.positiveY
--                     angle =
--                         Direction2d.toAngle dir
--                     radians =
--                         Angle.inRadians angle - (pi / 2)
--                 in
--                 shapeWithOptions
--                     rectangle
--                     { color = Color.red
--                     , position = ( car.x, car.y, 0 )
--                     , size = ( 20, 400 )
--                     , pivot = ( 1, 0 )
--                     , rotation = radians
--                     }
--             )
--             forces


debugLapTimer : List Car -> List Renderable
debugLapTimer cars =
    List.foldl
        (\car res ->
            let
                center =
                    LapTimer.nextPoint car.lapTimer
            in
            res ++ [ debugSpot Color.darkOrange center 200 ]
        )
        []
        cars


bodySpecsToRenderables : Resources -> List BodySpec -> List Renderable
bodySpecsToRenderables resources bodies =
    List.map
        (\body ->
            case body.type_ of
                "car" ->
                    -- shapeWithOptions circle
                    --     { color = Color.blue
                    --     , position = ( body.x, body.y, 0 )
                    --     , size = ( f body.width, f body.height )
                    --     , rotation = body.rotation
                    --     , pivot = ( 0.5, 0.5 )
                    --     }
                    spriteWithOptions
                        { texture = Resources.getTexture "./car.png" resources
                        , position = ( body.x, body.y, 0 )
                        , size = ( f body.width, f body.height )
                        , tiling = ( 1, 1 )
                        , rotation = body.rotation
                        , pivot = ( 0.5, 0.5 )
                        }

                _ ->
                    shapeWithOptions circle
                        { color = Color.darkRed
                        , position = ( body.x, body.y, 0 )
                        , size = ( f body.width, f body.height )
                        , rotation = body.rotation
                        , pivot = ( 0.5, 0.5 )
                        }
        )
        bodies



--model.objects
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Race mdl ->
            case mdl.raceState of
                Racing ->
                    batch
                        [ Ports.jsToElm passData
                        , onAnimationFrameDelta StepAnimation
                        , Time.every 100 (always UpdateTargetPoints)
                        ]
                _ ->
                    Sub.none

        _ ->
            Sub.none


passData : IncomingData -> Msg
passData incomingData =
    case incomingData.dataType of
        "UpdatedPhysics" ->
            case Json.Decode.decodeValue (Json.Decode.list decodeBodySpecs) incomingData.payload of
                Ok list ->
                    UpdatePhysics list

                Err msg ->
                    -- let
                    --     _ =
                    --         Debug.log "Failed parsing update physics" (Debug.toString msg)
                    -- in
                    NoOp

        dataType ->
            -- let
            --     _ =
            --         Debug.log "Unknown incomingData datatype" dataType
            -- in
            NoOp


decodeBodySpecs : Decoder BodySpec
decodeBodySpecs =
    Json.Decode.succeed BodySpec
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float
        |> required "width" Json.Decode.int
        |> required "height" Json.Decode.int
        |> required "rotation" Json.Decode.float
        |> required "mass" Json.Decode.int
        |> required "id" Json.Decode.string
        |> required "velocity" velocityDecoder
        |> required "type_" Json.Decode.string


velocityDecoder : Decoder Vector
velocityDecoder =
    Json.Decode.succeed Vector
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


arrayAsTuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
arrayAsTuple2 a b =
    Json.Decode.field "x" a
        |> Json.Decode.andThen
            (\aVal ->
                Json.Decode.field "y" b
                    |> Json.Decode.andThen (\bVal -> Json.Decode.succeed ( aVal, bVal ))
            )


debugTrack : Track -> Int -> List Renderable
debugTrack track trackWidth =
    let
        circles =
            Track.toCircles track

        rectangles =
            Track.toRectangles track
    in
    List.map
        (\circle2d ->
            let
                ( centerX, centerY ) =
                    Circle2d.centerPoint circle2d |> Point2d.toTuple Length.inMeters
            in
            shapeWithOptions
                circle
                { color = Color.lightRed
                , position = ( centerX, centerY, 0 )
                , size =
                    ( Circle2d.radius circle2d |> Length.inMeters, Circle2d.radius circle2d |> Length.inMeters )
                        |> Tuple.mapBoth ((*) 2) ((*) 2)
                , pivot = ( 0.5, 0.5 )
                , rotation = 0
                }
        )
        circles
        ++ List.map
            (\rectangle2d ->
                let
                    ( centerX, centerY ) =
                        Rectangle2d.centerPoint rectangle2d |> pointToTuple
                in
                shapeWithOptions
                    rectangle
                    { color = Color.lightRed
                    , position = ( centerX, centerY, 0 )
                    , size = Rectangle2d.dimensions rectangle2d |> tupleToFloatTuple
                    , pivot = ( 0.5, 0.5 )
                    , rotation = 0
                    }
            )
            rectangles


renderShadow : List Car -> List Renderable
renderShadow cars =
    let
        mCar =
            cars
                |> List.filter
                    (\car -> car.body.id == "car-1")
                |> List.head

        maybeSpot =
            case mCar of
                Just car ->
                    Shadow.getCurrentRenderSpot car.shadow

                Nothing ->
                    Nothing
    in
    case maybeSpot of
        Just spot ->
            [ debugSpot Color.charcoal spot.point 200 ]

        Nothing ->
            []
