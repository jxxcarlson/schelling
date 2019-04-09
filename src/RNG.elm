module RNG exposing (floatSequence, floatSequence_)
import Random
import List.Extra

gen : Int -> Random.Generator (List Float)
gen n =
    Random.list n (Random.float 0 1)

makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k

floatSequence : Int -> Int -> List Float
floatSequence n k =
    floatSequence_ n (makeSeed k)
      |> Tuple.first

floatSequence_ : Int -> Random.Seed -> (List Float, Random.Seed)
floatSequence_ n seed =
    Random.step (gen n) seed


intSequence : Int -> Int -> List Int
intSequence n k =
    floatSequence n k
      |> List.map (\x -> round (100000*x))

--intPairSequence : Int -> Int -> List (Int, Int)
--intPairSequence n seed =
--      List.Extra.splitAt n (intSequence (2*n) seed)
--        |>  List.map2 Tuple.pair