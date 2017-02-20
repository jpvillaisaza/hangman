module Tests exposing (..)

-- hangman
import Hangman exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "tests"
        [ isPlayingTests
        ]

isPlayingTests : Test
isPlayingTests =
    describe "isPlaying"
        [ test "isPlaying (1)" <|
            \() ->
              Expect.equal (isPlaying Lost) False
        , fuzz int "isPlaying (2)" <|
            \n ->
                Expect.equal (isPlaying (Playing n)) True
        , test "isPlaying (3)" <|
            \() ->
                Expect.equal (isPlaying Lost) False
        ]
