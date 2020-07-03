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
        floatCol =
            toFloat col

        floatRow =
            toFloat row

        floatGridWidth =
            toFloat gridWidth

        ( x, y ) =
            ( tileCenter floatCol floatGridWidth, tileCenter floatRow floatGridWidth )

        size =
            trackWidth
    in
    shapeWithOptions circle
        { color = Color.gray
        , position = ( x, y, 0 )
        , size = ( toFloat size, toFloat size )
        , rotation = 0
        , pivot = ( 0.5, 0.5 )
        }


tileCenter : Float -> Float -> Float
tileCenter index size =
    index * size + (size / 2)
