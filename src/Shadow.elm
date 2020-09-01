module Shadow exposing (..)


type alias ShadowSpot =
    { point : ( Float, Float )
    , rotation : Float
    , time : Float
    }


type Shadow
    = Shadow
        { best : List ShadowSpot
        , current : List ShadowSpot
        , currentRender : List ShadowSpot
        , currentRenderSpot : Maybe ShadowSpot
        , lastTime : Float
        }


empty : Shadow
empty =
    Shadow { best = [], current = [], currentRender = [], lastTime = 0, currentRenderSpot = Nothing }


rotate : Shadow -> Bool -> Shadow
rotate (Shadow shadow) bestLap =
    if bestLap then
        Shadow
            { best = List.reverse shadow.current
            , current = []
            , currentRender = List.reverse shadow.current
            , currentRenderSpot = List.head (List.reverse shadow.current)
            , lastTime = 0
            }

    else
        Shadow
            { shadow
                | current = []
                , currentRender = shadow.best
                , currentRenderSpot = List.head shadow.best
                , lastTime = 0
            }


addPoint : Shadow -> ( Float, Float ) -> Float -> Float -> Shadow
addPoint (Shadow shadow) point rotation time =
    if time - shadow.lastTime > 0.04 then
        Shadow { shadow | current = ShadowSpot point rotation time :: shadow.current, lastTime = time }
            |> updateShadowSpot time

    else
        Shadow shadow

getCurrentRenderSpot : Shadow -> Maybe ShadowSpot
getCurrentRenderSpot (Shadow shadow) =
    shadow.currentRenderSpot

updateShadowSpot : Float -> Shadow -> Shadow
updateShadowSpot time (Shadow shadow) =
    case List.head shadow.currentRender of
        Just spot ->
            let
                currentRender_ =
                    findNearest shadow.currentRender time
            in
            Shadow { shadow | currentRender = currentRender_, currentRenderSpot = List.head currentRender_ }

        Nothing ->
            Shadow shadow


findNearest : List ShadowSpot -> Float -> List ShadowSpot
findNearest spots time =
    case List.head spots of
        Just spot ->
            if spot.time > time then
                spots

            else
                findNearest (List.drop 1 spots) time

        Nothing ->
            []
