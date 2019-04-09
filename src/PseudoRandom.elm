module PseudoRandom exposing (integerSequence, floatSequence, modulus, roundTo)

{-

References:

  1. https://www.math.arizona.edu/~tgk/mc/book_chap3.pdf

  2. http://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/S0025-5718-99-00996-5.pdf

  We use the PRNG of (1), of Lewis, Goodman, and Miller,

-}

floatSequence : Int -> (Float, Float) -> List Float
floatSequence n (a,b) =
    let
        m = toFloat modulus
    in
    integerSequence n
      |> List.map (\x -> (b - a)*(toFloat x)/m + a)

integerSequence : Int -> List Int
integerSequence n =
    List.take n <| orbit ff n 23

modulus = 214748364

{-| Use this to produce not very good "random" sequences,
as in this example:

> orbit ff 10 3
[0.0751,0.833,0.7394,0.991,0.2915,0.8048,0.708,0.979,0.3553]
-}
orbit : (Maybe Int -> Int) -> Int ->  Int -> List Int
orbit f n seed  =
    orbitAux f (n+1) [seed]


orbitAux : (Maybe Int -> Int) -> Int -> List Int -> List Int
orbitAux f n ns =
    case n == 0 of
        True -> ns
        False -> orbitAux f (n - 1) ((f (List.head ns))::ns)

ff : Maybe Int -> Int
ff maybeInt =
    case maybeInt of
        Nothing -> modulus//2 + 1
        Just k -> modBy modulus (16807*k)

roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)

