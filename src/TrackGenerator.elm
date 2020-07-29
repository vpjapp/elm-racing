module TrackGenerator exposing (..)

import AStar exposing (findPath, straightLineCost)
import Array2D exposing (Array2D)
import Model exposing (CarControlPoint(..))
import Random exposing (initialSeed)
import Random.List exposing (..)
import Set exposing (Set)


maxWidth =
    7


maxHeight =
    7


type TrackTile
    = EmptyTile ( Int, Int )
    | TrackTile ( Int, Int )


type TrackResult
    = Track ( List ( Int, Int ), Random.Seed )
    | Fail Random.Seed


generateTrack : Int -> Maybe (List ( Int, Int ))
generateTrack seedInt =
    let
        array =
            Array2D.initialize maxHeight maxWidth (\r c -> EmptyTile ( r, c ))

        seed =
            initialSeed seedInt

        ( ( row, col ), seed2 ) =
            Random.step randomPoint seed

        array_ =
            Array2D.set row col (TrackTile ( col, row )) array

        track =
            getRecursiveTrack array_ ( row, col ) ( row, col ) 0 seed2
    in
    case track of
        Track ( res, _ ) ->
            Just res

        Fail _ ->
            Nothing


getRecursiveTrack : Array2D TrackTile -> ( Int, Int ) -> ( Int, Int ) -> Int -> Random.Seed -> TrackResult
getRecursiveTrack track startPoint currentPoint count seed =
    let
        _ =
            Debug.log "currentPoint" currentPoint
    in
    if currentPoint == startPoint && count > 0 then
        Track ( [], seed )

    else
        let
            sPoint =
                if count > 1 then
                    Just startPoint

                else
                    Nothing

            nextPossibilities =
                getNextPossiblePoints track currentPoint (count > 1) sPoint
                    |> Debug.log "NextPossibilities"

            ( shuffledNext, seed2 ) =
                Random.step (shuffle nextPossibilities) seed
        in
        List.foldl
            (\nextPoint trackRes ->
                (case trackRes of
                    Fail seed3 ->
                        let
                            ( col, row ) =
                                currentPoint

                            track_ =
                                Array2D.set row col (TrackTile ( col, row )) track

                            _ =
                                Debug.log "Searching path from " <| pointToString startPoint ++ pointToString ( col, row )

                            returnPath =
                                findPath
                                    straightLineCost
                                    (\pos -> getNextPossiblePoints track pos False (Just startPoint) |> Debug.log "Possible astar pos's" |> Set.fromList)
                                    ( col, row )
                                    startPoint
                                    |> Debug.log "Return path"

                            tres =
                                case returnPath of
                                    Just _ ->
                                        getRecursiveTrack track_ startPoint nextPoint (count + 1) seed3
                                            |> (\res ->
                                                    case res of
                                                        Track ( list, seedX ) ->
                                                            Track ( currentPoint :: list, seedX )

                                                        Fail seedF ->
                                                            Fail seedF
                                               )
                                            |> (Debug.log <| "Recursive call " ++ (String.fromInt <| count + 1))

                                    Nothing ->
                                        Fail seed3
                        in
                        tres

                    Track res ->
                        Track res
                )
                    |> Debug.log "Round fold"
            )
            (Fail seed2)
            shuffledNext


pointToString ( x, y ) =
    " (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


getPossiblePoint : Array2D TrackTile -> ( Int, Int ) -> ( Int, Int ) -> Bool -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
getPossiblePoint track point1 point2 canJump possibleStartPoint =
    -- TODO check that the jump can only go over a straight
    let
        pointIsStartPoint1 =
            case possibleStartPoint of
                Just point ->
                    point == point1

                Nothing ->
                    False

        pointIsStartPoint2 =
            case possibleStartPoint of
                Just point ->
                    point == point2

                Nothing ->
                    False
    in
    if pointIsStartPoint1 || tileIsEmpty track point1 then
        Just point1

    else if canJump && (pointIsStartPoint2 || tileIsEmpty track point2) then
        Just point2

    else
        Nothing


getNextPossiblePoints : Array2D TrackTile -> ( Int, Int ) -> Bool -> Maybe ( Int, Int ) -> List ( Int, Int )
getNextPossiblePoints track point canJump possibleStartPoint =
    let
        up =
            getPossiblePoint
                track
                (getPointUp point)
                (getPointUp <| getPointUp point)
                (canJump && isHorizontalStraight track (getPointUp <| getPointUp point))
                possibleStartPoint

        right =
            getPossiblePoint
                track
                (getPointRight point)
                (getPointRight <| getPointRight point)
                (canJump && isVerticalStraight track (getPointRight <| getPointRight point))
                possibleStartPoint

        down =
            getPossiblePoint
                track
                (getPointDown point)
                (getPointDown <| getPointDown point)
                (canJump && isHorizontalStraight track (getPointDown <| getPointDown point))
                possibleStartPoint

        left =
            getPossiblePoint
                track
                (getPointLeft point)
                (getPointLeft <| getPointLeft point)
                (canJump && isVerticalStraight track (getPointLeft <| getPointLeft point))
                possibleStartPoint
    in
    [ up, right, down, left ]
        |> List.filterMap identity


isHorizontalStraight : Array2D TrackTile -> Model.Point -> Bool
isHorizontalStraight track point =
    (getPointLeft point |> tileIsEmpty track |> not)
        && (getPointRight point |> tileIsEmpty track |> not)

isVerticalStraight : Array2D TrackTile -> Model.Point -> Bool
isVerticalStraight track point =
    (getPointUp point |> tileIsEmpty track |> not)
        && (getPointDown point |> tileIsEmpty track |> not)


getPointUp =
    getPoint ( 0, -1 )


getPointDown =
    getPoint ( 0, 1 )


getPointRight =
    getPoint ( 1, 0 )


getPointLeft =
    getPoint ( -1, 0 )


getPoint ( deltaX, deltaY ) ( x, y ) =
    ( x + deltaX, y + deltaY )


tileIsEmpty : Array2D TrackTile -> ( Int, Int ) -> Bool
tileIsEmpty array ( col, row ) =
    let
        mTile =
            Array2D.get row col array
    in
    case mTile of
        Just (TrackTile point) ->
            False

        Just (EmptyTile point) ->
            True

        Nothing ->
            False



-- Array2D.get row (col + 1) array |>
--     (\mPoint ->
--         case mPoint of
--             Just (TrackTile point) -> point)


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.map2
        (\x y -> ( x, y ))
        (Random.int 0 <| maxHeight - 1)
        (Random.int 0 <| maxWidth - 1)
