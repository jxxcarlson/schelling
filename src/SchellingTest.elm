module SchellingTest exposing (..)

import Schelling exposing(Cell(..), Id(..), Threshold(..), Identity(..), EmotionalState(..), CellIndex(..))
import Array



c0 =
    Unoccupied (CellIndex 0) (Id 0)

c0b = Occupied (CellIndex 0) (Id 0) (Threshold 0.3) Red Satisfied

c1 =
    Occupied (CellIndex 1) (Id 1) (Threshold 0.3) Red Satisfied


c2=
    Unoccupied (CellIndex 2) (Id 2)


c3 =
    Occupied (CellIndex 3) (Id 3) (Threshold 0.3) Blue Satisfied


c4 =
    Occupied (CellIndex 4) (Id 4) (Threshold 0.3) Blue Satisfied


c5 =
    Unoccupied (CellIndex 5) (Id 5)


c6 =
    Occupied (CellIndex 6)  (Id 6) (Threshold 0.3) Blue Satisfied


c7 =
    Unoccupied (CellIndex 7) (Id 7)


c8 =
    Occupied (CellIndex 8) (Id 8) (Threshold 0.4) Blue Satisfied

c8b =
    Occupied (CellIndex 8) (Id 8) (Threshold 0.4) Red Satisfied

c9 = Unoccupied (CellIndex 9)  (Id 9)

testCells =
    Array.fromList [ c0, c1, c2, c3, c4, c5, c6, c7, c8 ]

tc = testCells

testSequence n =
    List.map2 Tuple.pair (List.range 0 (n-1)) (List.range 0 (n-1))