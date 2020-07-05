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
import Track exposing (fromString, getHeight, getWidth)
import Vector2d


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Resources resMsg, Loading loadState ) ->
            setResources resMsg loadState |> (\mdl -> ( maybeNextState mdl, Cmd.none ))

        ( ResFail err, _ ) ->
            --let
            --    _ =
            --        Debug.todo "ResFail: " ++ err
            --in
            ( model, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( UpdatePhysics updatedBodies, Race mdl ) ->
            ( Race
                { mdl
                    | bodies = updatedBodies
                }
            , Cmd.none
            )

        ( AddBodies, mdl ) ->
            ( mdl, outgoingAddBodies )

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
                        mdl.targetPoint

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

                forces =
                    accelerateToTarget ++ slideControlForce

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

                camera =
                    fixedArea (w * h) ( w / 2, h / 2 )

                trackString =
                    String.replace " " "" "E0 E2 D2 D4 B4 B2 A2 A1 C1 C0"

                dummyTrack =
                    fromString 0 0 (String.replace " " "" trackString)

                -- 4x7
                ySize =
                    round h // getHeight dummyTrack

                xSize =
                    round w // getWidth dummyTrack

                gridSize =
                    min ySize xSize

                trackSize =
                    round <| toFloat gridSize * 0.9

                track =
                    fromString gridSize trackSize trackString
            in
            ( maybeNextState { mdl | dimensions = Just ( round w, round h ) }, Cmd.none )

        ( _, mdl ) ->
            ( mdl, Cmd.none )



-- ( { model | width = round w, height = round h, camera = camera, track = track }, Cmd.none )


maybeNextState mdl =
    case ( mdl.resources, mdl.dimensions ) of
        ( Just res, Just dimensions ) ->
            Menu { resources = res, dimensions = dimensions }

        ( _, _ ) ->
            Loading mdl


setResources resMsg maybeModel =
    { maybeModel
        | resources = Maybe.map (Resources.update resMsg) maybeModel.resources

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
calculateForceFromCarAndTarget (model) func =
    case model.targetPoint of
        Nothing ->
            [ { x = 0, y = 0 } ]

        Just ( x, y ) ->
            case List.head model.bodies of
                Nothing ->
                    []

                Just car ->
                    func car ( x, y )


calculateForceFromCar : RaceDetails -> (BodySpec -> List Vector) -> List Vector
calculateForceFromCar (model) func =
    case List.head model.bodies of
        Nothing ->
            [ { x = 0, y = 0 } ]

        Just car ->
            func car


setTargetPoint : Maybe ( Float, Float ) -> RaceDetails -> RaceDetails
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
    [ BodySpec 100 100 16 32 1.15 100 "car-1" { x = 0, y = 0 } "car"
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
