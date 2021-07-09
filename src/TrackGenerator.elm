module TrackGenerator exposing (..)

import AStar exposing (findPath, straightLineCost)
import Array2D exposing (Array2D)
import Model exposing (CarControl(..), Point)
import Random exposing (initialSeed)
import Random.List exposing (..)
import Set exposing (Set)


maxWidth =
    5


maxHeight =
    5


type TrackTile
    = EmptyTile ( Int, Int )
    | TrackTile ( Int, Int )


type TrackResult
    = Track ( List ( Int, Int ), Random.Seed )
    | Fail Random.Seed


type TileType
    = Point
    | Empty
    | OffGrid


generateTrack : Int -> Maybe (List ( Int, Int ))
generateTrack seedInt =
    let
        array =
            Array2D.initialize maxHeight maxWidth (\r c -> EmptyTile ( r, c ))

        seed =
            initialSeed seedInt

        ( ( col, row ), seed2 ) =
            Random.step getStartPoint seed

        ( isVert, seed3 ) =
            Random.step boolGen seed2

        -- TODO allow start straight to be vertical also
        array_ =
            Array2D.set row col (TrackTile ( col, row )) array

        track =
            getRecursiveTrack array_ ( col, row ) ( col, row ) 0 seed3
    in
    case track of
        Track ( res, _ ) ->
            Just <| normalizeTrack res

        Fail _ ->
            Nothing


getRecursiveTrack : Array2D TrackTile -> ( Int, Int ) -> ( Int, Int ) -> Int -> Random.Seed -> TrackResult
getRecursiveTrack track startPoint currentPoint count seed =
    if currentPoint == startPoint && count > 0 then
        -- Track is finished, time to return
        Track ( [], seed )

    else
        let
            sPoint =
                if count > 1 then
                    Just startPoint

                else
                    Nothing

            ( col, row ) =
                currentPoint

            nextPossibilities =
                if count < 2 then
                    -- Start straight
                    [ ( col, row + 1 ) ]

                else
                    getNextPossiblePoints track currentPoint (count > 1) sPoint

            -- Randomize the order of next possible tiles
            ( shuffledNext, seed2 ) =
                Random.step (shuffle nextPossibilities) seed
        in
        -- Iterate nextPossibilities in random order. Recurse to each possibility.
        -- If valid track has been found, then return it and ignore other possibly remaining nextPossibilities
        -- Start with default of Fail so if none of the nextPossibilities result in valid track, return Fail
        List.foldl
            (\nextPoint trackRes ->
                case trackRes of
                    Fail seed3 ->
                        let
                            -- Add current point to the current track
                            track_ =
                                Array2D.set row col (TrackTile ( col, row )) track

                            -- A* lookup from current point to the start (is it possible to find valid track from this point)
                            returnPath =
                                findPath
                                    straightLineCost
                                    (\pos -> getNextPossiblePoints track pos False (Just startPoint) |> Set.fromList)
                                    ( col, row )
                                    startPoint

                            tres =
                                case returnPath of
                                    -- Yes it is possible to find valid track, move to the next point and continue
                                    Just _ ->
                                        getRecursiveTrack track_ startPoint nextPoint (count + 1) seed3
                                            |> (\res ->
                                                    case res of
                                                        -- Valid track was found through this point, add this point to the return value
                                                        Track ( list, seedX ) ->
                                                            Track ( currentPoint :: list, seedX )

                                                        -- Could not find valid path through this point
                                                        Fail seedF ->
                                                            Fail seedF
                                               )

                                    -- Could not find valid track from this point. Back off and try other points
                                    Nothing ->
                                        Fail seed3
                        in
                        tres

                    -- Full track has been found, return the results, effectively skip the rest nextPossible points
                    Track res ->
                        Track res
            )
            (Fail seed2)
            shuffledNext


pointToString ( x, y ) =
    " (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


getPossiblePoint : Array2D TrackTile -> ( Int, Int ) -> ( Int, Int ) -> Bool -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
getPossiblePoint track point1 point2 canJump possibleStartPoint =
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
    if pointIsStartPoint1 || (tileType track point1 == Empty) then
        Just point1

    else if canJump && (pointIsStartPoint2 || tileType track point2 == Empty) then
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
    ((getPointLeft point |> tileType track) == Point)
        && ((getPointRight point |> tileType track) == Point)


isVerticalStraight : Array2D TrackTile -> Model.Point -> Bool
isVerticalStraight track point =
    ((getPointUp point |> tileType track) == Point)
        && ((getPointDown point |> tileType track) == Point)


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


tileType : Array2D TrackTile -> ( Int, Int ) -> TileType
tileType array ( col, row ) =
    let
        mTile =
            Array2D.get row col array
    in
    case mTile of
        Just (TrackTile point) ->
            Point

        Just (EmptyTile point) ->
            Empty

        Nothing ->
            OffGrid



-- Array2D.get row (col + 1) array |>
--     (\mPoint ->
--         case mPoint of
--             Just (TrackTile point) -> point)


getStartPoint : Random.Generator ( Int, Int )
getStartPoint =
    Random.map2
        (\x y -> ( x, y ))
        (Random.int 0 <| maxHeight - 1)
        (Random.int 0 <| maxWidth - 3)


boolGen : Random.Generator Bool
boolGen =
    Random.int 0 1
        |> Random.andThen
            (\nro ->
                if nro > 0 then
                    Random.constant True

                else
                    Random.constant False
            )


normalizeTrack : List Point -> List Point
normalizeTrack trackList =
    let
        minX =
            List.foldl min 10000 (List.map Tuple.first trackList)

        minY =
            List.foldl min 10000 (List.map Tuple.second trackList)

        trackList_ =
            List.map
                (\( x, y ) -> ( x - minX, y - minY ))
                trackList

        trackList__ =
            case List.head trackList_ of
                Just tile ->
                    List.drop 1 trackList_ ++ [ tile ]

                Nothing ->
                    trackList_
    in
    trackList__
