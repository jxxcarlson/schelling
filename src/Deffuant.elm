module Deffuant exposing (..)

import Array exposing(Array)


nAgents = 5

nRows =
    nAgents


nCols =
  nAgents

location : (Int, Int) -> Int
location (row, col) =
    nRows * row + col

indexTuple : Int -> (Int, Int)
indexTuple n =
  (n // nCols, modBy nCols n)

type alias OpinionMatrix = Array (Maybe Float)

opinionMatrixTest = Array.fromList [
     Nothing, Just 0.5, Nothing, Just 0.2, Just 0.8
   , Just 0.2, Nothing, Nothing, Just 0.5, Nothing
   , Nothing, Nothing, Nothing, Just 0.6, Nothing
   , Just 0.8, Just 0.2, Just 0.1, Nothing, Nothing
   , Just 0.4, Nothing, Nothing,  Nothing, Nothing
   ]

opt = opinionMatrixTest

get : (Int, Int) -> OpinionMatrix ->  Maybe Float
get (row, col) array =
    Array.get (location (row, col)) array |> Maybe.withDefault Nothing


set : (Int, Int) -> Maybe Float -> OpinionMatrix -> OpinionMatrix
set (row, col) cell opinionMatrix =
   Array.set (location (row, col)) cell opinionMatrix


getRow : Int -> OpinionMatrix ->  Array (Maybe Float)
getRow k array =
    Array.map (\col -> Array.get (location (k, col)) array |> Maybe.withDefault Nothing)
      (Array.fromList (List.range 0 (nCols - 1 )))




acquaintances : Int -> OpinionMatrix -> Array (Maybe Float)
acquaintances i om =
    getRow i om
      |> Array.filter (\value -> value  /= Nothing)

