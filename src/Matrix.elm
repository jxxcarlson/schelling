module Matrix exposing (nRows, nCols, location, indexTuple)


nRows =
    32


nCols =
   32

location : Int -> Int -> Int
location row col =
    nRows * row + col

indexTuple : Int -> (Int, Int)
indexTuple n =
    (n // nCols, modBy nCols n)