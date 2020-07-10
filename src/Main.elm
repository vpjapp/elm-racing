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
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Length exposing (Length)
import Model exposing (..)
import Platform.Sub exposing (batch)
import Point2d exposing (Point2d)
import Ports exposing (..)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Task
import Track exposing (..)
import TrackUtils exposing (pointToTuple, tupleToFloatTuple)
import Update exposing (update)
import Vector2d



-- TODO state transfer from loading -> menu and menu button clicking and setting up the race after click.
---- MODEL ----


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
                [ Pointer.onDown (relativePos >> Just >> SetTargetPoint)
                , Touch.onMove (touchCoordinates >> Just >> SetTargetPoint)
                , Pointer.onMove (relativePos >> Just >> SetTargetPoint)
                , Pointer.onUp (\_ -> SetTargetPoint Nothing)
                , Pointer.onOut (\_ -> SetTargetPoint Nothing)
                , Pointer.onLeave (\_ -> SetTargetPoint Nothing)
                ]
                { time = 0
                , size = mdl.dimensions
                , camera = mdl.camera
                }
                (toRenderables mdl.track
                    ++ debugForces (getCar mdl) mdl.forces
                    -- ++ debugSpots mdl.dimensions
                    ++ debugTargetPoint mdl.car.targetPoint
                    -- ++ debugTrack mdl.track 2000
                    ++ debugOnTrack mdl.car
                    ++ bodySpecsToRenderables mdl.resources mdl.bodies
                )

            -- , button [ onClick StepTime ] [ text "StepTime" ]
            -- , div [] (renderDebug model)
            ]

        Menu mdl ->
            [ div [] [ button [ onClick AddBodies ] [ text "Start drivin'" ] ] ]

        Loading mdl ->
            [ h1 [] [ text "Loading... Please wait." ] ]


getTileSize : Int -> Int -> Int
getTileSize pixels tileSize =
    pixels // tileSize


debugOnTrack car =
    [ debugSpot
        (if car.onTrack then
            Color.green

         else
            Color.red
        )
        ( car.body.x, car.body.y )
        400
    ]


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


debugTargetPoint : Maybe ( Float, Float ) -> List Renderable
debugTargetPoint mTarget =
    case mTarget of
        Nothing ->
            []

        Just ( x, y ) ->
            [ debugSpot Color.darkRed ( x, y ) 100
            ]


debugSpots ( width, height ) =
    [ debugSpot Color.brown ( 0, 0 )
    , debugSpot Color.orange ( f width, 0 )
    , debugSpot Color.blue ( f width, f height )
    , debugSpot Color.yellow ( 0, f height )
    ]


f =
    toFloat


debugSpot color ( x, y ) size =
    shapeWithOptions
        circle
        { color = color
        , position = ( x, y, 0 )
        , size = ( size, size )
        , pivot = ( 0.5, 0.5 )
        , rotation = 0.0
        }


debugForces : Maybe BodySpec -> List Vector -> List Renderable
debugForces mBodySpec forces =
    case mBodySpec of
        Nothing ->
            []

        Just car ->
            List.map
                (\{ x, y } ->
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
                    shapeWithOptions
                        rectangle
                        { color = Color.red
                        , position = ( car.x, car.y, 0 )
                        , size = ( 20, 400 )
                        , pivot = ( 1, 0 )
                        , rotation = radians
                        }
                )
                forces


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
    batch
        [ Ports.jsToElm passData
        , onAnimationFrameDelta StepAnimation
        ]


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
