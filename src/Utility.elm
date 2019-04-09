module Utility exposing (roundTo, pad)


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

