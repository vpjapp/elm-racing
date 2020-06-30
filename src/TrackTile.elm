module TrackTile exposing
    ( Dir(..)
    , TrackTile
    , UnresolvedTrackTile(..)
    , crossTile
    , downLeftTile
    , downRightTile
    , emptyTile
    , fromChar
    , getNextDir
    , horizontalTile
    , isDir
    , isPossibleStartTile
    , solved
    , unsolved
    , upLeftTile
    , upRightTile
    , validateByDir
    , validateLeftRight
    , validateUpDown
    , verticalTile
    )

import Dict exposing (..)
import Vector2d exposing (cross)


type alias TrackTileDirections =
    { up : Bool
    , right : Bool
    , down : Bool
    , left : Bool
    }


type Dir
    = Up
    | Right
    | Down
    | Left


getDirections : TrackTile -> List Dir
getDirections (TrackTile dirs) =
    appendIf dirs.up Up []
        |> appendIf dirs.right Right
        |> appendIf dirs.down Down
        |> appendIf dirs.left Left


getNextDir : TrackTile -> Dir -> Dir
getNextDir tile fromDir =
    let
        dirs =
            getDirections tile
                |> List.filter (\dir -> dir /= fromDir)
    in
    if List.length dirs == 1 then
        List.head dirs |> Maybe.withDefault Right

    else if tile == crossTile then
        getOpposite fromDir

    else
        Up


getOpposite : Dir -> Dir
getOpposite dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


appendIf : Bool -> a -> List a -> List a
appendIf cond item list =
    if cond then
        item :: list

    else
        list


isDir : Dir -> TrackTile -> Bool
isDir dir (TrackTile dirs) =
    case dir of
        Up ->
            dirs.up

        Right ->
            dirs.right

        Down ->
            dirs.down

        Left ->
            dirs.left


type TrackTile
    = TrackTile TrackTileDirections


type UnresolvedTrackTile
    = Solved TrackTile
    | Unsolved ( TrackTile, TrackTile )


solved a =
    Solved a


unsolved a b =
    Unsolved ( a, b )


emptyTile : TrackTile
emptyTile =
    TrackTile (TrackTileDirections False False False False)


verticalTile : TrackTile
verticalTile =
    TrackTile (TrackTileDirections True False True False)


horizontalTile : TrackTile
horizontalTile =
    TrackTile (TrackTileDirections False True False True)


upRightTile : TrackTile
upRightTile =
    TrackTile (TrackTileDirections True True False False)


upLeftTile : TrackTile
upLeftTile =
    TrackTile (TrackTileDirections True False False True)


downRightTile : TrackTile
downRightTile =
    TrackTile (TrackTileDirections False True True False)


downLeftTile : TrackTile
downLeftTile =
    TrackTile (TrackTileDirections False False True True)


crossTile : TrackTile
crossTile =
    TrackTile (TrackTileDirections True True True True)


validateByDir : TrackTile -> TrackTile -> Dir -> Bool
validateByDir tile nextTile dir =
    case dir of
        Up ->
            validateUpDown nextTile tile

        Right ->
            validateLeftRight tile nextTile

        Down ->
            validateUpDown tile nextTile

        Left ->
            validateLeftRight nextTile tile


validateLeftRight : TrackTile -> TrackTile -> Bool
validateLeftRight leftTile rightTile =
    let
        left =
            toDirs leftTile

        right =
            toDirs rightTile
    in
    (left.right && right.left) || (not left.right && not right.left)


validateUpDown : TrackTile -> TrackTile -> Bool
validateUpDown upTile downTile =
    let
        up =
            toDirs upTile

        down =
            toDirs downTile
    in
    (up.down && down.up) || (not up.down && not down.up)


toDirs : TrackTile -> TrackTileDirections
toDirs tile =
    case tile of
        TrackTile dirs ->
            dirs


fromChar : Char -> UnresolvedTrackTile
fromChar char =
    let
        tileMap : Dict Char UnresolvedTrackTile
        tileMap =
            Dict.fromList
                [ ( '/', Unsolved ( upLeftTile, downRightTile ) )
                , ( '-', Solved horizontalTile )
                , ( '\\', Unsolved ( upRightTile, downLeftTile ) )
                , ( '|', Solved verticalTile )
                , ( '+', Solved crossTile )
                , ( ' ', Solved emptyTile )
                ]

        maybeTile =
            Dict.get char tileMap
    in
    Maybe.withDefault (Solved emptyTile) maybeTile



-- resolveTile : UnresolvedTrackTile -> UnresolvedTrackTile -> UnresolvedTrackTile -> TrackTile
-- resolveTile tile right down =
--     case tile of
--         Solved t ->
--             t
--         Unsolved (opt1, opt2) ->


isPossibleStartTile : UnresolvedTrackTile -> Bool
isPossibleStartTile tile =
    case tile of
        Solved solvedTile ->
            let
                _ =
                    Debug.log ("found start tile " ++ boolStr (solvedTile /= emptyTile && solvedTile /= crossTile)) solvedTile
            in
            solvedTile /= emptyTile && solvedTile /= crossTile

        Unsolved _ ->
            False


boolStr bool =
    if bool then
        "True"

    else
        "False"



{-

   /\./-\
   |\\\\|
   \\\\||
   .\\\/|
   ..\--/

-}
