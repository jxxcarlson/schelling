module Utility exposing (roundTo, orbit, ff, pad)

modulus = 104729

roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)


{-|
> pad 16 [1,2,3]
[1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1]
-}
pad : Int -> List a -> List a
pad n xs =
    let
        blockSize = List.length xs
        numberOfBlocks = n//blockSize + 1
        xss = List.repeat numberOfBlocks xs |> List.concat
    in
    List.take n xss


{-| Use this to produce not very good "random" sequences,
as in this example:

> orbit ff 10 3
[0.0751,0.833,0.7394,0.991,0.2915,0.8048,0.708,0.979,0.3553]
-}
orbit : (Maybe Int -> Int) -> Int ->  Int -> List Float
orbit f n seed  =
    orbitAux f (n+1) [seed]
      |> List.map (\k -> roundTo 4 <| (toFloat k)/modulus)
      |> List.take (n - 1)

orbitAux : (Maybe Int -> Int) -> Int -> List Int -> List Int
orbitAux f n ns =
    case n == 0 of
        True -> ns
        False -> orbitAux f (n - 1) ((f (List.head ns))::ns)

ff : Maybe Int -> Int
ff maybeInt =
    case maybeInt of
        Nothing -> modulus//2 + 1
        Just k -> modBy modulus (74571*k + 20000)
