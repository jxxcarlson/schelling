module SchellingBenchmarks exposing (..)

import Array
import Benchmark exposing (..)
import Schelling exposing(..)
import Utility

-- cells = Schelling.initialize 0.4 0.1 0.5 (Utility.orbit Utility.ff (2*nRows*nCols) 23)

n = nRows*nCols


randList = (Utility.orbit Utility.ff (2*nRows*nCols) 23)
             |> List.map (\x -> round (100000*x))

tupleList = (Schelling.inputSequence n randList)

suite : Benchmark
suite =
    let
        cells = Schelling.initialize 0.4 0.1 0.5 (Utility.orbit Utility.ff (2*nRows*nCols) 23)
    in
    describe "Schelling"
        [ -- nest as many descriptions as you like
          describe "slice"
            [ benchmark "from the beginning" <|
                \_ -> updateCells tupleList cells

        ]
      ]