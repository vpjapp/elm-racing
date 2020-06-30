module TrackTests exposing (..)

import Expect
import PointTrack exposing (..)
import Test exposing (..)


fromStringTest : Test
fromStringTest =
    describe "fromString test"
        [ test "fromString A1"
            (\_ -> Expect.equal (stringToTupleList (String.toList "A5c5c0b0")) [ ( 0, 5 ), ( 2, 5 ), ( 2, 0 ), ( 1, 0 ) ])
        , test "fromString 2"
            (\_ -> Expect.equal (stringToTupleList (String.toList "AAc5c0b0g")) [ ( 0, 0 ), ( 2, 5 ), ( 2, 0 ), ( 1, 0 ),(6,-1) ])
        ]


isValidTest : Test
isValidTest =
    describe "isValid"
        [ test "isValid true"
            (\_ -> Expect.equal (fromString "a1b2c3d0" |> isValid) True)
        , test "isValid false"
            (\_ -> Expect.equal (fromString "a1r2c3d0" |> isValid) False)
        , test "isValid false 2"
            (\_ -> Expect.equal (fromString "a1b2c3d0d" |> isValid) False)
        ]
