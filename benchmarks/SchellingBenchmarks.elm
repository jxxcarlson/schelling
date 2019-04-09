module SchellingBenchmarks exposing (..)

import Array
import Benchmark exposing (..)
import Schelling exposing(..)
import Utility
import PseudoRandom
import RNG
import List.Extra

-- cells = Schelling.initialize 0.4 0.1 0.5 (Utility.orbit Utility.ff (2*nRows*nCols) 23)



--
--randList = (Utility.orbit Utility.ff (2*nRows*nCols) 23)
--             |> List.map (\x -> round (100000*x))
--
--tupleList = (Schelling.inputSequence n randList)


suite : Benchmark
suite =
    describe "Schelling"
        [ -- nest as many descriptions as you like
          describe "random nunber generators"
            [
              benchmark "Pseudo" <|
                 \_ -> PseudoRandom.floatSequence 10 23 (0,1)

             , benchmark "Random" <|
                  \_ -> RNG.floatSequence 10 23

             , benchmark "make Cells" <|
                  \_ -> Schelling.initialize defaultModel (RNG.floatSequence 1000 23)

              , benchmark "update Cells" <|
                  \_ -> Schelling.updateCells defaultModel (List.Extra.splitAt 1000 (RNG.floatSequence 2000 23))
             ]
         ]

updateCells model tupleList cellArray
--
--suite : Benchmark
--suite =
--    let
--        cells = Schelling.initialize 0.4 0.1 0.5 (Utility.orbit Utility.ff (2*nRows*nCols) 23)
--    in
--    describe "Schelling"
--        [ -- nest as many descriptions as you like
--          describe "slice"
--            [ benchmark "from the beginning" <|
--                \_ -> updateCells tupleList cells
--
--        ]
--      ]