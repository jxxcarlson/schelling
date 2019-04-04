module SchellingTest exposing (..)

import Schelling exposing(Cell(..), Id(..), Threshold(..), Identity(..), EmotionalState(..))
import Array



c0 =
    Unoccupied (Id 0)

c0b = Occupied (Id 0) (Threshold 0.3) Red Satisfied

c1 =
    Occupied (Id 1) (Threshold 0.3) Red Satisfied


c2=
    Unoccupied (Id 2)


c3 =
    Occupied (Id 3) (Threshold 0.3) Blue Satisfied


c4 =
    Occupied (Id 4) (Threshold 0.3) Blue Satisfied


c5 =
    Unoccupied (Id 5)


c6 =
    Occupied (Id 6) (Threshold 0.3) Blue Satisfied


c7 =
    Unoccupied (Id 7)


c8 =
    Occupied (Id 8) (Threshold 0.4) Blue Satisfied

c8b =
    Occupied (Id 8) (Threshold 0.4) Red Satisfied

c9 = Unoccupied (Id 9)

testCells =
    Array.fromList [ c0, c1, c2, c3, c4, c5, c6, c7, c8 ]


testSequence n =
    List.map2 Tuple.pair (List.range 0 (n-1)) (List.range 0 (n-1))