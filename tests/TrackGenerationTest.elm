module TrackGenerationTest exposing (..)

import Expect
import Test exposing (..)
import TrackGenerator exposing (generateTrack)


testTrack : Test
testTrack =
    describe "Track generation"
        [ test "Track gen 1"
            (\_ ->
                Expect.equal
                    (generateTrack 2)
                    (Just
                        [ ( 4, 4 )
                        , ( 4, 5 )
                        , ( 4, 6 )
                        , ( 3, 6 )
                        , ( 3, 5 )
                        , ( 3, 4 )
                        , ( 3, 3 )
                        , ( 2, 3 )
                        , ( 2, 4 )
                        , ( 2, 5 )
                        , ( 2, 6 )
                        , ( 1, 6 )
                        , ( 0, 6 )
                        , ( 0, 5 )
                        , ( 1, 5 )
                        , ( 1, 4 )
                        , ( 0, 4 )
                        , ( 0, 3 )
                        , ( 1, 3 )
                        , ( 1, 2 )
                        , ( 0, 2 )
                        , ( 0, 1 )
                        , ( 1, 1 )
                        , ( 1, 0 )
                        , ( 2, 0 )
                        , ( 3, 0 )
                        , ( 3, 1 )
                        , ( 4, 1 )
                        , ( 4, 0 )
                        , ( 4, 2 )
                        , ( 4, 3 )
                        ]
                    )
            )
        ]
