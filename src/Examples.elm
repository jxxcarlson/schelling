module Examples exposing (..)

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

rands = PseudoRandom.floatSequence n (0,1)

cells = Schelling.initialize  testModel rands

