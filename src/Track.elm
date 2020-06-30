{-
NOTE! THIS approach has been abandoned in favor of PointTrack.eml
-}

module Track exposing (Track, findStartCoords, fromString, getLongestLineLength, rightPadStringRows)

import Array2D exposing (Array2D, columns, fromList)
import List
import TrackTile
    exposing
        ( Dir(..)
        , TrackTile
        , UnresolvedTrackTile(..)
        , emptyTile
        , getNextDir
        , isPossibleStartTile
        , solved
        , unsolved
        , validateByDir
        , verticalTile
        )


type Track
    = Track
        { tiles : Array2D TrackTile
        , width : Int
        , height : Int
        }


fromString : String -> Result String Track
fromString trackString =
    let
        stringRows =
            String.split "\n" trackString

        maxLineLength =
            getLongestLineLength stringRows

        paddedStringRows : List String
        paddedStringRows =
            rightPadStringRows maxLineLength stringRows

        unresolvedTrack : Array2D UnresolvedTrackTile
        unresolvedTrack =
            Array2D.fromList (createUnresolvedTrack paddedStringRows)

        resolvedTrack : Maybe (Array2D TrackTile)
        resolvedTrack =
            resolveTrack unresolvedTrack maxLineLength (Array2D.columns unresolvedTrack)

        -- tiles = List.map (\row -> List.map TrackTile.fromChar (String.toList row)) paddedStringRows
        -- String.toList
    in
    Result.Ok (Track { tiles = Array2D.empty, width = maxLineLength, height = List.length paddedStringRows })


resolveTrack : Array2D UnresolvedTrackTile -> Int -> Int -> Maybe (Array2D TrackTile)
resolveTrack unresolvedTrack width height =
    let
        trackTiles : Array2D TrackTile
        trackTiles =
            Array2D.repeat width height emptyTile

        -- TODO
        -- - find non-empty tile
        -- - start going throught the track recursively
        -- - recursive func like: x -> y -> Tile -> Dir -> Array2D -> Step# -> Maybe Array2D
        -- - If Unresolved, follow both possible tracks until one is found to be valid
        -- - Stop when back in the tile where started or steps are over the size of the grid
        startCoords : Maybe ( Int, Int )
        startCoords =
            findStartCoords unresolvedTrack ( 0, 0 )

        track =
            case startCoords of
                Just ( row, col ) ->
                    let
                        initDir =
                            getInitDir (Array2D.get row col unresolvedTrack)
                    in
                    Just <| resolveTrackStartingFrom ( row, col ) initDir unresolvedTrack trackTiles

                Nothing ->
                    Nothing
    in
    Nothing



-- track


getInitDir : Maybe UnresolvedTrackTile -> Dir
getInitDir unsolvedTile =
    case unsolvedTile |> Maybe.withDefault (solved verticalTile) of
        Solved tile ->
            if tile == verticalTile then
                Up

            else
                Right

        _ ->
            Right


getTile : ( Int, Int ) -> Array2D UnresolvedTrackTile -> UnresolvedTrackTile
getTile ( row, col ) track =
    Array2D.get row col track |> Maybe.withDefault (Solved emptyTile)


resolveTrackStartingFrom : ( Int, Int ) -> Dir -> Array2D UnresolvedTrackTile -> Array2D TrackTile -> Maybe (Array2D TrackTile)
resolveTrackStartingFrom ( row, column ) fromDir unresolvedTrack resultTrack =
    let
        unsolvedTile =
            getTile ( row, column ) unresolvedTrack



        unsolvedNextTile =
            getTile ( nextRow, nextCol ) unresolvedTrack
    in
    case unsolvedTile of
        Solved tile ->
            let
                nextDir =
                    getNextDir tile fromDir
                ( nextRow, nextCol ) =
                    getCoords ( row, column ) nextDir
            in
            case unsolvedNextTile of
                Solved nextTile ->
                    if validateByDir tile nextTile fromDir then
                        resolveTrackStartingFrom ( nextRow, nextCol ) nextDir unresolvedTrack (Array2D.set row column tile resultTrack)

                    else
                        Nothing

                -- Unsolved may only be 2 corner tiles and one of them matches always
                Unsolved (ig, nore) ->
                    resolveTrackStartingFrom ( nextRow, nextCol ) nextDir unresolvedTrack (Array2D.set row column tile resultTrack)


        Unsolved ( tile1, tile2 ) ->
            let
                tile1NextDir = tile1 fromDir
                tile2NextDir = tile2 fromDir

            in



getCoords : ( Int, Int ) -> Dir -> ( Int, Int )
getCoords ( row, col ) dir =
    case dir of
        Up ->
            ( row - 1, col )

        Right ->
            ( row, col + 1 )

        Down ->
            ( row + 1, col )

        Left ->
            ( row, col - 1 )


findStartCoords : Array2D UnresolvedTrackTile -> ( Int, Int ) -> Maybe ( Int, Int )
findStartCoords array ( row, column ) =
    case Array2D.get row column array of
        Just unsolvedTile ->
            if isPossibleStartTile unsolvedTile then
                Just ( row, column )

            else
                let
                    nextColumn =
                        modBy (Array2D.columns array) (column + 1)

                    nextRow =
                        if nextColumn == 0 then
                            row + 1

                        else
                            row

                    overLimits =
                        nextRow > Array2D.rows array
                in
                if overLimits then
                    Nothing

                else
                    findStartCoords array ( nextRow, nextColumn )

        Nothing ->
            Nothing


createUnresolvedTrack : List String -> List (List UnresolvedTrackTile)
createUnresolvedTrack stringRows =
    List.map
        (\stringRow ->
            List.map TrackTile.fromChar stringRow
        )
        (List.map String.toList stringRows)


getLongestLineLength : List String -> Int
getLongestLineLength rows =
    List.foldl (\line longest -> max longest (String.length line)) 0 rows


rightPadStringRows : Int -> List String -> List String
rightPadStringRows maxLineLength stringRows =
    List.map (String.padRight maxLineLength ' ') stringRows
