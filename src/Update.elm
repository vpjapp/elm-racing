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


allTrackTiles : List TrackTile
allTrackTiles =
    [ Empty
    , Vertical
    , Horizontal
    , UpRight
    , UpLeft
    , DownRight
    , DownLeft
    , Cross
    ]


generateAllTracs : Int -> Int -> List Track
generateAllTracs width height =
    []


generateTiles : Int -> Int -> Int -> List (List TrackTile)
generateTiles width height currentIndex =
    if currentIndex > 0 then
        let
            tileLists : List (List TrackTile)
            tileLists =
                List.foldl
                    (\tile tileListList ->
                        tileListList
                            ++ List.map
                                (\list -> tile :: list)
                                (generateTiles width height (currentIndex - 1))
                    )
                    []
                    allTrackTiles
                    |> (\tracks -> List.filter (\track -> validateFirstTile track width height) tracks)
        in
        tileLists

    else
        [ [] ]


leftBorderTiles =
    [ Empty, Vertical, UpRight, DownRight ]


rightBorderTiles =
    [ Empty, Vertical, UpLeft, DownLeft ]


topBorderTiles =
    [ Empty, Horizontal, DownRight, DownLeft ]


bottomBorderTiles =
    [ Empty, Horizontal, UpRight, UpLeft ]


isBottomBorder : Int -> Int -> Int -> Bool
isBottomBorder length width height =
    length <= width


isTopBorder : Int -> Int -> Int -> Bool
isTopBorder length width height =
    length > width * (height - 1)


isLeftBorder : Int -> Int -> Int -> Bool
isLeftBorder length width height =
    modBy width length == 0


isRightBorder : Int -> Int -> Int -> Bool
isRightBorder length width height =
    modBy width length == 1



-- TODO check that first tile is valid according to second tile, tile under and if tile is border tile,
-- check that track does not go over board.e


validateFirstTile : List TrackTile -> Int -> Int -> Bool
validateFirstTile track width height =
    case track of
        tile :: rest ->
            let
                length =
                    List.length track

                prevTile =
                    List.head rest

                belowTile =
                    getBelow track width

                belowValid =
                    isBelowValid tile belowTile

                rightValid =
                    isRightValid tile <| List.head rest

                topBorderValid =
                    (not <| isTopBorder length width height)
                        || List.member tile topBorderTiles

                bottomBorderValid =
                    (not <| isBottomBorder length width height)
                        || List.member tile bottomBorderTiles

                rightBorderValid =
                    (not <|
                        isRightBorder length width height
                    )
                        || List.member tile rightBorderTiles

                leftBorderValid =
                    (not <|
                        isLeftBorder length width height
                    )
                        || List.member tile leftBorderTiles

                allValid =
                    topBorderValid
                        && bottomBorderValid
                        && rightBorderValid
                        && leftBorderValid
                        && belowValid
                        && rightValid

                _ =
                    if not allValid then
                        ( tile
                        , belowTile
                        , [ topBorderValid
                          , bottomBorderValid
                          , rightBorderValid
                          , leftBorderValid
                          , belowValid
                          , rightValid
                          ]
                        )

                    else
                        ( tile, belowTile, [ True ] )
            in
            allValid

        [] ->
            True


upTiles =
    [ Vertical, UpLeft, UpRight, Cross ]


rightTiles =
    [ Horizontal, UpRight, DownRight, Cross ]


downTiles =
    [ Vertical, DownLeft, DownRight, Cross ]


leftTiles =
    [ Horizontal, UpLeft, DownLeft, Cross ]


getBelow : List TrackTile -> Int -> Maybe TrackTile
getBelow track width =
    let
        length =
            List.length track

        tile =
            if length < width then
                Nothing

            else
                List.drop width track |> List.head
    in
    tile


isBelowValid : TrackTile -> Maybe TrackTile -> Bool
isBelowValid top below =
    case below of
        Nothing ->
            True

        Just belowTile ->
            bothOrNeither top downTiles belowTile upTiles



-- (List.member top downTiles && List.member belowTile upTiles)
--     || ((not <| List.member top downTiles) && (not <| List.member belowTile upTiles))


isRightValid : TrackTile -> Maybe TrackTile -> Bool
isRightValid left right =
    case right of
        Nothing ->
            True

        Just rightTile ->
            bothOrNeither left rightTiles rightTile leftTiles


bothOrNeither : TrackTile -> List TrackTile -> TrackTile -> List TrackTile -> Bool
bothOrNeither tile tileList belowTile belowTileList =
    (List.member tile tileList && List.member belowTile belowTileList)
        || ((not <| List.member tile tileList) && (not <| List.member belowTile belowTileList))



{-
   4x3
   x x 10 9
   8 7 6  5
   4 3 2  1

   4x3:
   x x 1 2
   3 4 5 6
   7 8 9 10

   3x2:
   6 5 4
   3 2 1

   3x3
   9 8 7
   6 5 4
   3 2 1
-}
