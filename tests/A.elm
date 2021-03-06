module A exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Schelling exposing(..)
import List.Extra as LE
import PseudoRandom


testModel = {
   nRows = 40
  , nCols = 40
  , threshold = 0.4
  , probabilityOfUnoccupied = 0.1
  , probabilityOfRed = 0.5
  , cellSize = 8
 }

n = testModel.nRows*testModel.nCols

rands = PseudoRandom.floatSequence (2*n) (0,1)

cells = Schelling.initialize  testModel rands

-- [Just 0.0617, Just 0.9688,Just 0.8465 ]


suite : Test
suite =
    describe "The Schelling module"
        [ describe "CellMatrix operations"
            [ test "Check integrity of rands"  <|

               \_ ->
                     Expect.equal [Just 0.6971619164465439, Just 0.8857613881519488 ] [LE.getAt 500 rands, LE.getAt 600 rands]
             , test "Get a cell by address" <|
                \_ ->
                    let
                        a00 = Unoccupied (CellIndex 0) (Id 0)
                        b00 = get testModel (0,0) cells
                    in
                        Expect.equal b00 a00
             , test "Get another  cell by address" <|
                  \_ ->
                      let
                          a01 = Occupied (CellIndex 1) (Id 1) (Threshold 0.4) Red Satisfied
                          b01 = get testModel (0,1) cells
                      in
                          Expect.equal a01 b01
             , test "Swap cells I, check identities" <|
                  \_ ->
                      let
                          a00 = Unoccupied (CellIndex 0) (Id 0)
                          a01 = Occupied (CellIndex 1) (Id 1) (Threshold 0.4) Red Satisfied
                          cells2 = swapCells a00 a01 cells
                          b00 = get testModel (0,0) cells2
                      in
                          Expect.equal (Schelling.identity a01) (Schelling.identity b00)
             , test "Swap cells II, check identities" <|
                  \_ ->
                      let
                          a00 = Unoccupied (CellIndex 0) (Id 0)
                          a01 = Occupied (CellIndex 1) (Id 1) (Threshold 0.4) Red Satisfied
                          cells2 = swapCells a00 a01 cells
                          b01 = get testModel (0,1) cells2
                      in
                          Expect.equal (Schelling.identity a00) (Schelling.identity b01)

            , test "Check emotional state before" <|
                  \_ ->
                      let
                          cell = get testModel (0,1) cells
                      in
                          Expect.equal (emotionalState cell) Satisfied

           , test "Check emotional state after" <|
                  \_ ->
                      let
                          cell = updateEmotionalStateOfCellAtIndex testModel (0,1) cells
                      in
                          Expect.equal (emotionalState cell) Unsatisfied

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]