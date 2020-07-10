module Update exposing (..)

import Angle
import Direction2d
import Game.Resources as Resources
import Game.TwoD.Camera as Camera exposing (fixedArea, fixedWidth)
import Game.TwoD.Render exposing (..)
import Json.Encode exposing (..)
import Length
import Model exposing (..)
import Point2d
import Ports exposing (..)
import Quantity
import Track exposing (fromString, getHeight, getWidth, startPoint)
import Vector2d


f =
    toFloat


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

        ( UpdatePhysics updatedBodies, Race mdl ) ->
            let
                newCarBody =
                    List.head updatedBodies

                oldCar =
                    mdl.car
            in
            case newCarBody of
                Just theCarBody ->
                    let
                        ( targetX, targetY ) =
                            ( theCarBody.x, theCarBody.y )

                        updatedCar =
                            { oldCar
                                | body = theCarBody
                                , onTrack = Track.onTrack mdl.track ( targetX, targetY )
                            }
                    in
                    ( Race
                        { mdl
                            | bodies = updatedBodies
                            , car = updatedCar
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Race
                        { mdl
                            | bodies = updatedBodies
                        }
                    , Cmd.none
                    )

        ( AddBodies, Menu mdl ) ->
            let
                ( w, h ) =
                    mdl.dimensions

                trackString =
                    String.replace " " "" "E0 E2 D2 D4 B4 B2 A2 A1 C1 C0"

                dummyTrack =
                    fromString 0 0 (String.replace " " "" trackString)

                gridSize =
                    2500

                trackSize =
                    2000

                -- 4x7
                ySize =
                    gridSize * getHeight dummyTrack

                xSize =
                    gridSize * getWidth dummyTrack

                camera =
                    fixedWidth (f xSize) ( f xSize / 2, f ySize / 2 )

                track =
                    fromString gridSize trackSize trackString

                trackStart =
                    startPoint track

                car =
                    createCar trackStart
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
                , car = car
                }
            , outgoingAddBodies [ car.body ]
            )

        ( StepTime, mdl ) ->
            ( mdl, outgoingStepTime 1.1 [] )

        ( SetTargetPoint point, Race mdl ) ->
            let
                targetPoint =
                    if mdl.toggler then
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

                    else
                        mdl.car.targetPoint

                -- debug =
                --     addDebug model.debug
                --         (Debug.toString targetPoint ++ ", " ++ Debug.toString point)
            in
            ( Race ({ mdl | toggler = not mdl.toggler } |> setTargetPoint targetPoint), Cmd.none )

        ( StepAnimation delta, Race mdl ) ->
            let
                accelerateToTarget =
                    getTargetAcceleration mdl

                slideControlForce =
                    slideControl mdl

                offTrackDragForce =
                    offTrackDrag mdl.car

                forces =
                    accelerateToTarget
                        ++ slideControlForce
                        ++ offTrackDragForce

                toggler =
                    True

                --not model.toggler
            in
            if toggler then
                ( Race { mdl | forces = forces, toggler = toggler }, outgoingStepTime delta forces )

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

        ( _, mdl ) ->
            ( mdl, Cmd.none )


offTrackDrag : Car -> List Vector
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

            speed: Float
            speed = Vector2d.length velocity |> Length.inMeters

            dragForce = speed * speed / 2000

            vector =
                Vector2d.withLength (Length.meters dragForce) direction
        in
        [ { x = Vector2d.xComponent vector |> Length.inMeters, y = Vector2d.yComponent vector |> Length.inMeters } ]

    else
        []


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


getTargetAcceleration : RaceDetails -> List Vector
getTargetAcceleration model =
    calculateForceFromCarAndTarget model <|
        \car ( x, y ) ->
            let
                targetPoint =
                    Point2d.unitless x y

                carPos =
                    Point2d.unitless car.x car.y

                vector =
                    Vector2d.from carPos targetPoint
                        |> Vector2d.normalize
                        |> Vector2d.scaleBy 0.3
            in
            [ Vector2d.toUnitless vector ]


slideControl : RaceDetails -> List Vector
slideControl model =
    calculateForceFromCar model <|
        \car ->
            let
                faceDirection =
                    Direction2d.fromAngle (Angle.radians car.rotation)

                sideDirection =
                    {- Direction2d.rotateClockwise -}
                    faceDirection

                velocity =
                    Vector2d.fromRecord Quantity.float car.velocity

                sideVelocityValue =
                    Vector2d.componentIn sideDirection velocity

                slideFactor =
                    -0.0004

                forceVector =
                    Vector2d.withLength (Quantity.multiplyBy slideFactor sideVelocityValue) sideDirection
            in
            [ Vector2d.toUnitless forceVector ]


calculateForceFromCarAndTarget : RaceDetails -> (BodySpec -> ( Float, Float ) -> List Vector) -> List Vector
calculateForceFromCarAndTarget model func =
    case model.car.targetPoint of
        Nothing ->
            [ { x = 0, y = 0 } ]

        Just ( x, y ) ->
            func model.car.body ( x, y )


calculateForceFromCar : RaceDetails -> (BodySpec -> List Vector) -> List Vector
calculateForceFromCar model func =
    func model.car.body


setTargetPoint : Maybe ( Float, Float ) -> RaceDetails -> RaceDetails
setTargetPoint point model =
    let
        car =
            model.car
    in
    { model | car = { car | targetPoint = point } }


outgoingStepTime : Float -> List Vector -> Cmd Msg
outgoingStepTime delta list =
    elmToJs <| OutgoingData "PhysicsUpdate" (Just <| encodeStepTime delta list)


encodeStepTime : Float -> List Vector -> Value
encodeStepTime delta forces =
    object <|
        [ ( "delta", float delta )
        , ( "forces", list encodeVector2 forces )
        ]


outgoingAddBodies : List BodySpec -> Cmd Msg
outgoingAddBodies bodies =
    elmToJs <| OutgoingData "AddBodies" (Just (bodies |> encodeBodies))


createCar : ( Int, Int ) -> Car
createCar startPoint =
    { body = BodySpec (Tuple.first startPoint |> f) (Tuple.second startPoint |> f) 200 400 1.15 100 "car-1" { x = 0, y = 0 } "car"
    , targetPoint = Nothing
    , onTrack = True
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
