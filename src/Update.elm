module Update exposing (..)

import Angle
import Direction2d
import Game.Resources as Resources
import Game.TwoD.Camera as Camera exposing (fixedArea)
import Game.TwoD.Render exposing (..)
import Json.Encode exposing (..)
import Length
import Model exposing (..)
import Point2d
import Ports exposing (..)
import Quantity
import Vector2d


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resources resMsg ->
            ( { model
                | resources = Resources.update resMsg model.resources
                , objects =
                    spriteWithOptions
                        { texture = Resources.getTexture "./car.png" model.resources
                        , position = ( 150, -200, 0 )
                        , size = ( 64, 128 )
                        , tiling = ( 0, 0 )
                        , rotation = 2.1
                        , pivot = ( 0.5, 0.5 )
                        }
                        :: model.objects
              }
            , Cmd.none
            )

        ResFail err ->
            --let
            --    _ =
            --        Debug.todo "ResFail: " ++ err
            --in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UpdatePhysics updatedBodies ->
            ( { model
                | bodies = updatedBodies
              }
            , Cmd.none
            )

        AddBodies ->
            ( model, outgoingAddBodies )

        StepTime ->
            ( model, outgoingStepTime 1.1 [] )

        SetTargetPoint point ->
            let
                targetPoint =
                    if model.toggler then
                        Maybe.map
                            (\( x, y ) ->
                                Camera.viewportToGameCoordinates
                                    model.camera
                                    ( model.width, model.height )
                                    ( round x
                                    , round y
                                    )
                            )
                            point

                    else
                        model.targetPoint

                --                debug =
                --                    addDebug model.debug
                --                        (Debug.toString targetPoint ++ ", " ++ Debug.toString point)
            in
            ( { model | toggler = not model.toggler } |> setTargetPoint targetPoint, Cmd.none )

        StepAnimation delta ->
            let
                accelerateToTarget =
                    getTargetAcceleration model

                slideControlForce =
                    slideControl model

                forces =
                    accelerateToTarget ++ slideControlForce

                toggler =
                    True

                --not model.toggler
            in
            if toggler then
                ( { model | forces = forces, toggler = toggler }, outgoingStepTime delta forces )

            else
                ( { model | toggler = toggler }, Cmd.none )

        SetScreenSize viewport ->
            let
                w =
                    viewport.viewport.width

                h =
                    viewport.viewport.height

                camera =
                    fixedArea (w * h) ( w / 2, h / 2 )
            in
            ( { model | width = round w, height = round h }, Cmd.none )


addDebug : List String -> String -> List String
addDebug oldList debug =
    debug :: List.take 10 oldList


getTargetAcceleration : Model -> List Vector
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
            in
            [ Vector2d.toUnitless vector ]


slideControl : Model -> List Vector
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


calculateForceFromCarAndTarget : Model -> (BodySpec -> ( Float, Float ) -> List Vector) -> List Vector
calculateForceFromCarAndTarget model func =
    case model.targetPoint of
        Nothing ->
            [ { x = 0, y = 0 } ]

        Just ( x, y ) ->
            case List.head model.bodies of
                Nothing ->
                    []

                Just car ->
                    func car ( x, y )


calculateForceFromCar : Model -> (BodySpec -> List Vector) -> List Vector
calculateForceFromCar model func =
    case List.head model.bodies of
        Nothing ->
            [ { x = 0, y = 0 } ]

        Just car ->
            func car


setTargetPoint : Maybe ( Float, Float ) -> Model -> Model
setTargetPoint point model =
    { model | targetPoint = point }


outgoingStepTime : Float -> List Vector -> Cmd Msg
outgoingStepTime delta list =
    elmToJs <| OutgoingData "PhysicsUpdate" (Just <| encodeStepTime delta list)


encodeStepTime : Float -> List Vector -> Value
encodeStepTime delta forces =
    object <|
        [ ( "delta", float delta )
        , ( "forces", list encodeVector2 forces )
        ]


outgoingAddBodies : Cmd Msg
outgoingAddBodies =
    elmToJs <| OutgoingData "AddBodies" (Just (getInitialBodies |> encodeBodies))


getInitialBodies : List BodySpec
getInitialBodies =
    [ BodySpec 600 600 16 32 1.15 100 "car-1" { x = 0, y = 0 } "car"
    , createCircle 100 100
    , createCircle 300 300
    , createCircle 800 600
    , createCircle 800 400
    , createCircle 200 400
    , createCircle 200 600
    , createCircle 800 200
    ]


createCircle posX posY =
    BodySpec posX posY 16 16 0 180000 "circle-2" { x = 0, y = 0 } "circle"


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
