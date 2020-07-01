module TrackRenderables exposing (..)

import Color
import Game.TwoD.Render exposing (Renderable, circle, shapeWithOptions)


getRenderables : Int -> Int -> List ( Int, Int ) -> List Renderable
getRenderables gridWidth trackWidth points =
    List.map
        (getCircles
            gridWidth
            trackWidth
        )
        points


getCircles : Int -> Int -> ( Int, Int ) -> Renderable
getCircles gridWidth trackWidth ( row, col ) =
    let
        ( x, y ) =
            ( col * gridWidth, row * gridWidth )

        size =
            trackWidth
    in
    shapeWithOptions circle
        { color = Color.gray
        , position = ( toFloat x, toFloat y, 0 )
        , size = ( toFloat size, toFloat size )
        , rotation = 0
        , pivot = ( 0.5, 0.5 )
        }
