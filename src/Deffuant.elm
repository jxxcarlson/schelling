module Deffuant exposing (..)

import Array exposing(Array)
import List.Extra
import Array.Extra
import Cards



--
-- MODEL PARAMETERS
--

nAgents : Int
nAgents = 5

nRows : Int
nRows =
    nAgents

nCols : Int
nCols =
  nAgents

kModel : Int
kModel = 2

rho : Float
rho = 1

sigma : Float
sigma = 1

omega : Float
omega = 0.5

delta : Float
delta = 0.1

--
-- MATRIX STUFF
--

location : (Int, Int) -> Int
location (row, col) =
    nRows * row + col

indexTuple : Int -> (Int, Int)
indexTuple n =
  (n // nCols, modBy nCols n)

type alias OpinionMatrix = Array (Maybe Float)

--
-- TEST DATA
--

opinionMatrixTest = Array.fromList [
     Nothing, Just 0.5, Nothing, Just 0.2, Just 0.8
   , Just 0.2, Nothing, Nothing, Just 0.5, Nothing
   , Nothing, Nothing, Nothing, Just 0.6, Nothing
   , Just 0.8, Just 0.2, Just 0.1, Nothing, Nothing
   , Just 0.4, Nothing, Nothing,  Nothing, Nothing
   ]

opt = opinionMatrixTest

floats = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4]

--
-- MATRIX OPERATIONS
--


get : (Int, Int) -> OpinionMatrix ->  Maybe Float
get (row, col) array =
    Array.get (location (row, col)) array |> Maybe.withDefault Nothing

get2 : (Int, Int) -> OpinionMatrix ->  Float
get2 (row, col) array =
    case get(row, col) array of
        Nothing -> 0
        Just x -> x

numberAt : Int -> (List Float) -> Float
numberAt k list =
    List.Extra.getAt k list |> Maybe.withDefault 0.0

set : (Int, Int) -> Maybe Float -> OpinionMatrix -> OpinionMatrix
set (row, col) cell opinionMatrix =
   Array.set (location (row, col)) cell opinionMatrix


getRow : Int -> OpinionMatrix ->  Array (Maybe Float)
getRow k array =
    Array.map (\col -> Array.get (location (k, col)) array |> Maybe.withDefault Nothing)
      (Array.fromList (List.range 0 (nCols - 1 )))


--
-- INTERACTION
--


interaction : (Int, Int) -> (List Float) -> OpinionMatrix -> OpinionMatrix
interaction (i, j) rnds om =
    let
      om1 = interactonPhase1 (i, j) om
      aii = get2 (i,i) om
      ajj = get2 (j,j) om
      aij = get2 (i,j) om
      aji = get2 (j,i) om
      pij = pc (i,j) om
      aiiNew = aii + pij * ( aji - aii + (numberAt 0 rnds))
      aijNew = aij + pij * ( ajj - aij + (numberAt 1 rnds))
      rnds2 = List.drop 2 rnds
      om2 = set (i,i) (Just aiiNew) om1
      om3 = set (i,j) (Just aijNew) om2
      -- Acquaintances
      aqIndices = getIndices (\x -> x /= Nothing) (getRow i om) |> Array.toList
      aqIndices2 = randomizeIndices (List.take 4 rnds2) aqIndices |> List.take kModel
      rnds3 = List.drop 4  rnds2
      aqRands = List.take (List.length aqIndices) rnds3
      rnds4 = List.drop (List.length aqIndices) rnds3
      aqData = List.map2 (\p r -> (p,r)) aqIndices2 aqRands |> Debug.log "aqData"
      newOpinions = List.map (opinionOfAcquaintance (i,j) om3 pij) aqData |> Debug.log "newOpinions"
      om4 = List.foldl (\(indexPair,opinion) omx -> set indexPair opinion omx) om3 newOpinions
      -- Vanity
      aij2 = get2 (i,j) om4
      aji2 = get2 (j,i) om4
      aii2 = get2 (i,i) om4
      aij2New = aij2 + omega * (aji2 - aii2 + headNumber rnds4)
      om5 = set (i,j) (Just aij2New) om4
    in
     om5

headNumber : List Float -> Float
headNumber list =
    case List.head list of
        Nothing -> 0
        Just x -> x
{-|
  opinionOfAcquaintance (0,1) opt 1 (4,0.3)
  -}
opinionOfAcquaintance : (Int, Int) -> OpinionMatrix -> Float -> (Int, Float) -> ((Int, Int), Maybe Float)
opinionOfAcquaintance (i,j) om pij (q, rnd) =
    let
       aiq = get2 (i,q) om
       ajq = get2 (j,q) om
    in
    ((i, q), Just (aiq + pij*rho*(ajq - aiq + rnd)))


randomizeIndices : List Float -> List Int -> List Int
randomizeIndices rnds indices =
    let
       randInts = List.take 4 rnds |> List.map (\x -> round (1001*x) )
    in
      Cards.randomizeList randInts indices


getIndices : (a -> Bool) -> Array a -> Array Int
getIndices f array  =
    array
      |> Array.indexedMap Tuple.pair
      |> Array.filter (\(i, x) -> f x)
      |> Array.map Tuple.first



{-| Select subarry -}
select : List Int -> Array a -> Array a
select indices array =
    array
      |> Array.indexedMap Tuple.pair
      |> Array.filter(\(i, x) -> List.member i indices)
      |> Array.map Tuple.second


acquaintances : Int -> OpinionMatrix -> Array (Maybe Float)
acquaintances i om =
    getRow i om
      |> Array.filter (\value -> value  /= Nothing)



interactonPhase1 : (Int, Int) -> OpinionMatrix -> OpinionMatrix
interactonPhase1 (i, j) om =
    let
        myself = get (i,i) om
        her = get(j,j) om
        om1 = case myself of
                 Nothing -> set (i,i) (Just 0) om
                 Just _ -> om
        om2 = case her of
                 Nothing -> set (j,j) (Just 0) om1
                 Just _ -> om1
    in
      om2

pc : (Int, Int) -> OpinionMatrix -> Float
pc (i,j) om =
    let
        aii = get2 (i,i) om
        aij = get2 (i,j) om
        num = (aii - aij)/sigma
    in
      1.0/(1 + e^num)

