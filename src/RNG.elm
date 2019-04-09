module RNG exposing (floatSequence, floatSequence_)
import Random
import List.Extra

{-

Example:

> RNG.floatSequence 3 23 (0,1)
[0.07049563320325747,0.8633668118636881,0.6762363032990798]

-}

gen : Int -> (Float, Float) -> Random.Generator (List Float)
gen n  (a, b) =
    Random.list n (Random.float a b)

makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k

floatSequence : Int -> Int -> (Float, Float) -> List Float
floatSequence n k (a,b) =
    floatSequence_ n (makeSeed k) (a,b)
      |> Tuple.first

floatSequence_ : Int -> Random.Seed -> (Float, Float) -> (List Float, Random.Seed)
floatSequence_ n seed (a,b) =
    Random.step (gen n (a,b)) seed

--
--intSequence : Int -> Int -> List Int
--intSequence n k =
--    floatSequence n k
--      |> List.map (\x -> round (100000*x))

--intPairSequence : Int -> Int -> List (Int, Int)
--intPairSequence n seed =
--      List.Extra.splitAt n (intSequence (2*n) seed)
--        |>  List.map2 Tuple.pair