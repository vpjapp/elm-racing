module Update exposing (..)

import AiCarLogic exposing (nextPointLogic, nextTwoPointLogic, nextTwoPointLogicWithBraking)
import Angle
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Direction2d
import Game.Resources as Resources
import Game.TwoD.Camera as Camera exposing (Camera, fixedHeight, fixedWidth)
import Game.TwoD.Render exposing (..)
import Json.Encode exposing (..)
import LapTimer exposing (..)
import Length exposing (inMeters)
import Model exposing (..)
import Point2d exposing (Point2d)
import Ports exposing (..)
import Process
import Quantity
import Task as Task exposing (perform)
import Time as Time exposing (now)
import Track exposing (fromString, fromTuples, getHeight, getWidth, startPoint)
import TrackGenerator exposing (generateTrack)
import TrackUtils exposing (debugSpot, f, pointToIntTuple, pointToTuple)
import Vector2d


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Resources resMsg, Loading loadState ) ->
            setResources resMsg loadState
                |> (\mdl -> ( maybeNextState mdl, Cmd.none ))

        ( Resources resMsg, Menu loadState ) ->
            setResources resMsg loadState
                |> (\mdl -> ( Menu { loadState | resources = mdl.resources }, Cmd.none ))

        ( ResFail err, _ ) ->
            --let
            --    _ =
            --        Debug.todo "ResFail: " ++ err
            --in
            ( model, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( StartGeneratingTrackAndCars trackNro, Menu mdl ) ->
            ( LoadingTrack mdl, Process.sleep 100 |> Task.perform (\_ -> GenerateTrackAndCars trackNro) )

        ( GenerateTrackAndCars trackNro, LoadingTrack mdl ) ->
            let
                ( w, h ) =
                    mdl.dimensions

                dummyTrack =
                    fromTuples 0 0 (generateTrack trackNro |> Maybe.withDefault [])

                gridSize =
                    2500

                trackSize =
                    2000

                -- 4x7
                ySize =
                    gridSize * getHeight dummyTrack

                xSize =
                    gridSize * getWidth dummyTrack

                screenRatio =
                    f w / f h

                trackRatio =
                    f xSize / f ySize

                camera =
                    if screenRatio < trackRatio then
                        fixedWidth (f xSize) ( f xSize / 2, f ySize / 2 )

                    else
                        fixedHeight (f ySize) ( f xSize / 2, f ySize / 2 )

                -- track =
                --     fromString gridSize trackSize trackString
                track =
                    fromTuples gridSize trackSize (generateTrack trackNro |> Maybe.withDefault [])

                trackStart =
                    startPoint track

                ( ccX, ccY ) =
                    Camera.viewportToGameCoordinates
                        camera
                        mdl.dimensions
                        ( w - 200
                        , h - 200
                        )

                carControlPoint =
                    Point2d.meters ccX ccY

                -- carControlPoint =
                --     Point2d.meters ((w - 1) * gridSize |> f) (round gridSize |> f)
                carControlCircle =
                    Circle2d.withRadius (Length.meters 300) carControlPoint

                lapTimer =
                    LapTimer.timer (Track.toRectangles track)

                car =
                    -- createCar trackStart gridSize (Point { point = carControlPoint, circle = carControlCircle })
                    createCar "car-1" trackStart gridSize CursorToSelf lapTimer

                aiCar1 =
                    createCar "ai-car-1" trackStart gridSize (AiControl nextPointLogic) (LapTimer.timer (Track.toRectangles track))

                aiCar2 =
                    createCar "ai-car-2" trackStart gridSize (AiControl nextTwoPointLogicWithBraking) (LapTimer.timer (Track.toRectangles track))

                aiCar3 =
                    createCar "ai-car-3" trackStart gridSize (AiControl nextTwoPointLogic) (LapTimer.timer (Track.toRectangles track))

                -- car2 = createCar "car-2" trackStart gridSize Self lapTimer
                -- car3 = createCar "car-3" trackStart gridSize Self lapTimer
                cars =
                    [ car
                    , aiCar1
                    , aiCar2
                    , aiCar3
                    ]
            in
            ( Race
                { camera = camera
                , track = track
                , dimensions = mdl.dimensions
                , resources = mdl.resources
                , toggler = False
                , objects = []
                , debug = []
                , forces = []
                , bodies = []
                , cars = cars
                , raceState = Starting 5
                }
            , Cmd.batch [ outgoingAddBodies (List.map .body cars), Process.sleep 1000 |> Task.perform (\_ -> CountDown 4) ]
            )

        ( StepTime, mdl ) ->
            ( mdl, outgoingStepTime 1.1 [] )

        ( UpdatePhysics updatedBodies, Race mdl ) ->
            let
                newCarBodies =
                    List.filter (\body -> body.type_ == "car") updatedBodies

                otherBodies =
                    List.filter (\body -> body.type_ /= "car") updatedBodies

                oldCars =
                    mdl.cars

                updatedCars =
                    List.filterMap
                        (\newCarBody ->
                            let
                                oldCar =
                                    List.filter (\oc -> oc.body.id == newCarBody.id) oldCars |> List.head
                            in
                            case oldCar of
                                Just oc ->
                                    let
                                        ( targetX, targetY ) =
                                            ( newCarBody.x, newCarBody.y )

                                        updatedCar =
                                            { oc
                                                | body = newCarBody
                                                , onTrack = Track.onTrack mdl.track ( targetX, targetY )
                                            }
                                    in
                                    Just updatedCar

                                Nothing ->
                                    Nothing
                        )
                        newCarBodies

                carBodies =
                    List.map .body updatedCars
            in
            ( Race
                { mdl
                    | bodies = carBodies ++ otherBodies
                    , cars = updatedCars
                }
            , Cmd.none
            )

        ( SetTargetPoint carId point, Race mdl ) ->
            case mdl.raceState of
                Starting _ ->
                    ( Race mdl, Cmd.none )

                Finished ->
                    ( Race mdl, Cmd.none )

                Racing ->
                    let
                        targetPoint =
                            -- if mdl.toggler then
                            Maybe.map
                                (\( x, y ) ->
                                    Camera.viewportToGameCoordinates
                                        mdl.camera
                                        mdl.dimensions
                                        ( round x
                                        , round y
                                        )
                                )
                                point

                        -- else
                        --     mdl.car.targetPoint
                        -- debug =
                        --     addDebug model.debug
                        --         (Debug.toString targetPoint ++ ", " ++ Debug.toString point)
                    in
                    ( Race
                        ({ mdl | toggler = not mdl.toggler }
                            |> setTargetPoint targetPoint carId
                        )
                    , Cmd.none
                    )

        ( StepAnimation delta, Race mdl ) ->
            let
                gappedDelta =
                    min delta 100

                forces : List ( String, Vector )
                forces =
                    getCarForces mdl.cars

                -- skidMarks : List Renderable
                -- skidMarks =
                --     getSkidMarks forces mdl.cars
                toggler =
                    True

                cars =
                    List.map
                        (\car ->
                            let
                                lapTimer =
                                    LapTimer.update car.lapTimer ( car.body.x, car.body.y ) 16
                            in
                            { car | lapTimer = lapTimer }
                        )
                        mdl.cars

                --not model.toggler
            in
            if toggler then
                ( Race
                    { mdl
                        | forces = forces
                        , toggler = toggler
                        , cars = cars

                        -- , objects = mdl.objects ++ skidMarks
                    }
                , outgoingStepTime gappedDelta forces
                )

            else
                ( Race { mdl | toggler = toggler }, Cmd.none )

        ( SetScreenSize viewport, Loading mdl ) ->
            let
                w =
                    viewport.viewport.width

                h =
                    viewport.viewport.height
            in
            ( maybeNextState { mdl | dimensions = Just ( round w, round h ) }, Cmd.none )

        ( UpdateTargetPoints, Race mdl ) ->
            case mdl.raceState of
                Starting _ ->
                    ( Race mdl, Cmd.none )

                Finished ->
                    ( Race mdl, Cmd.none )

                Racing ->
                    ( mdl |> updateTargetPoint, Cmd.none )

        ( CountDown number, Race mdl ) ->
            if number == 0 then
                ( Race { mdl | raceState = Racing }, Cmd.none )

            else
                ( Race { mdl | raceState = Starting <| number - 1 }, Process.sleep 1000 |> Task.perform (\_ -> CountDown <| number - 1) )

        ( _, mdl ) ->
            ( mdl, Cmd.none )


getSkidMarks : List ( String, Vector ) -> List Car -> List Renderable
getSkidMarks forces cars =
    List.filterMap
        (\( id, force ) ->
            List.filter (\c -> c.body.id == id) cars
                |> List.head
                |> Maybe.andThen
                    (\car ->
                        let
                            velocity =
                                getVelocity force
                        in
                        if velocity > 0.30001 then
                            Just <| debugSpot Color.charcoal ( car.body.x, car.body.y ) 50

                        else
                            Nothing
                    )
        )
        forces


getCarForces : List Car -> List ( String, Vector )
getCarForces cars =
    List.map
        (\car ->
            let
                accelerateToTarget =
                    getTargetAcceleration car

                slideControlForce =
                    slideControl car

                offTrackDragForce =
                    offTrackDrag car
            in
            ( car.body.id, addVectors [ accelerateToTarget, slideControlForce, offTrackDragForce ] )
        )
        cars


addVectors : List Vector -> Vector
addVectors vectors =
    List.foldl
        (\cur res ->
            { x = cur.x + res.x
            , y = cur.y + res.y
            }
        )
        { x = 0, y = 0 }
        vectors


offTrackDrag : Car -> Vector
offTrackDrag car =
    if not car.onTrack then
        let
            { x, y } =
                car.body.velocity

            point =
                Point2d.meters x y

            direction =
                Direction2d.from Point2d.origin point
                    |> Maybe.withDefault (Direction2d.fromAngle (Angle.degrees 0))
                    |> Direction2d.reverse

            velocity =
                Vector2d.from Point2d.origin (Point2d.xy (Length.meters x) (Length.meters y))

            speed : Float
            speed =
                Vector2d.length velocity |> Length.inMeters

            dragForce =
                speed * speed / 2000

            vector =
                Vector2d.withLength (Length.meters dragForce) direction
        in
        { x = Vector2d.xComponent vector |> Length.inMeters, y = Vector2d.yComponent vector |> Length.inMeters }

    else
        Vector 0 0


getVelocity : Vector -> Float
getVelocity { x, y } =
    Vector2d.from Point2d.origin (Point2d.xy (Length.meters x) (Length.meters y)) |> Vector2d.length |> inMeters


maybeNextState mdl =
    case mdl.dimensions of
        Just dim ->
            Menu { resources = mdl.resources, dimensions = dim }

        _ ->
            Loading mdl


setResources resMsg model =
    { model
        | resources = Resources.update resMsg model.resources

        -- , objects =
        --     spriteWithOptions
        --         { texture = Resources.getTexture "./car.png" model.resources
        --         , position = ( 150, 150, 0 )
        --         , size = ( 32, 64 )
        --         , tiling = ( 0, 0 )
        --         , rotation = 2.1
        --         , pivot = ( 0.5, 0.5 )
        --         }
        --         :: model.objects
    }


addDebug : List String -> String -> List String
addDebug oldList debug =
    debug :: List.take 10 oldList


getTargetAcceleration : Car -> Vector
getTargetAcceleration car =
    case car.targetPoint of
        Nothing ->
            { x = 0, y = 0 }

        Just ( x, y ) ->
            let
                targetPoint =
                    Point2d.meters x y

                carPos =
                    case car.carControl of
                        CursorToSelf ->
                            Point2d.meters car.body.x car.body.y

                        CursorToPoint details ->
                            details.point

                        AiControl _ ->
                            Point2d.meters car.body.x car.body.y

                vector =
                    Vector2d.from carPos targetPoint
                        |> Vector2d.normalize
                        |> Vector2d.scaleBy 0.3
            in
            Vector2d.toUnitless vector


slideControl : Car -> Vector
slideControl car =
    let
        faceDirection =
            Direction2d.fromAngle (Angle.radians car.body.rotation)

        sideDirection =
            {- Direction2d.rotateClockwise -}
            faceDirection

        velocity =
            Vector2d.fromRecord Quantity.float car.body.velocity

        sideVelocityValue =
            Vector2d.componentIn sideDirection velocity

        slideFactor =
            -0.0004

        forceVector =
            Vector2d.withLength (Quantity.multiplyBy slideFactor sideVelocityValue) sideDirection
    in
    Vector2d.toUnitless forceVector



-- calculateForceFromCar : RaceDetails -> (BodySpec -> List Vector) -> List Vector
-- calculateForceFromCar model func =
--     func model.car.body


setTargetPoint : Maybe ( Float, Float ) -> String -> RaceDetails -> RaceDetails
setTargetPoint point carId model =
    let
        cars =
            List.map
                (\c ->
                    if c.body.id == carId then
                        { c | targetPoint = point }

                    else
                        c
                )
                model.cars
    in
    { model | cars = cars }


outgoingStepTime : Float -> List ( String, Vector ) -> Cmd Msg
outgoingStepTime delta list =
    elmToJs <| OutgoingData "PhysicsUpdate" (Just <| encodeStepTime delta list)


encodeStepTime : Float -> List ( String, Vector ) -> Value
encodeStepTime delta forces =
    object <|
        [ ( "delta", float delta )
        , ( "forces", list encodeIdVector2 forces )
        ]


outgoingAddBodies : List BodySpec -> Cmd Msg
outgoingAddBodies bodies =
    elmToJs <| OutgoingData "AddBodies" (Just (bodies |> encodeBodies))


createCar : String -> ( Int, Int ) -> Int -> CarControl -> LapTimer -> Car
createCar id startIndex gridWidth carControl lapTimer =
    { body =
        BodySpec
            (f (Tuple.second startIndex) * f gridWidth + 0.5 * f gridWidth)
            (f (Tuple.first startIndex) * f gridWidth + 0.5 * f gridWidth)
            200
            400
            1.15
            100
            id
            --"car-1"
            { x = 0, y = 0 }
            "car"
    , targetPoint = Nothing
    , onTrack = True
    , carControl = carControl
    , lapTimer = lapTimer
    }


getInitialBodies : List BodySpec
getInitialBodies =
    [ createCircle 100 100
    , createCircle 300 300
    , createCircle 800 600
    , createCircle 800 400
    , createCircle 200 400
    , createCircle 200 600
    , createCircle 800 200
    ]


createCircle posX posY =
    BodySpec posX posY 100 100 0 1800 "circle-2" { x = 0, y = 0 } "circle"


updateTargetPoint : RaceDetails -> Model
updateTargetPoint mdl =
    let
        cars =
            mdl.cars
                |> List.map
                    (\car ->
                        case car.carControl of
                            AiControl logic ->
                                { car | targetPoint = logic car }

                            _ ->
                                car
                    )
    in
    Race { mdl | cars = cars }



-- Encoders


encodeBodies : List BodySpec -> Value
encodeBodies bodyList =
    list encodeBodySpec bodyList


encodeBodySpec : BodySpec -> Value
encodeBodySpec bodySpec =
    object <|
        [ ( "x", float bodySpec.x )
        , ( "y", float bodySpec.y )
        , ( "width", int bodySpec.width )
        , ( "height", int bodySpec.height )
        , ( "rotation", float bodySpec.rotation )
        , ( "mass", int bodySpec.mass )
        , ( "id", string bodySpec.id )
        , ( "velocity", encodeVector2 bodySpec.velocity )
        , ( "type_", string bodySpec.type_ )
        ]


encodeVector : ( Float, Float ) -> Value
encodeVector ( x, y ) =
    object <|
        [ ( "x", float x )
        , ( "y", float y )
        ]


encodeVector2 : Vector -> Value
encodeVector2 vec =
    object <|
        [ ( "x", float vec.x )
        , ( "y", float vec.y )
        ]


encodeIdVector2 : ( String, Vector ) -> Value
encodeIdVector2 idVec =
    object <|
        [ ( "id", string (Tuple.first idVec) )
        , ( "force", encodeVector2 (Tuple.second idVec) )
        ]
