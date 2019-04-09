module PseudoRandom exposing (integerSequence, urandSequence, floatSequence, m0, roundTo, mod1)

{-

References:

  1. https://www.math.arizona.edu/~tgk/mc/book_chap3.pdf

  2. http://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/S0025-5718-99-00996-5.pdf

  We use the PRNG of (1), of Lewis, Goodman, and Miller,

-}

floatSequence : Int -> (Float, Float) -> List Float
floatSequence n (a,b) =
    urandSequence n
      |> List.map (\x -> (b - a)*x + a)

integerSequence : Int -> List Int
integerSequence n =
    List.take n <| orbit f0 n 23

integerSequence_ : (Maybe Int -> Int) -> Int -> List Int
integerSequence_ f n =
    List.take n <| orbit f n 1

urandSequence : Int -> List Float
urandSequence n =
    let
        s1 = integerSequence_ f1 n
        s2 = integerSequence_ f2 n
        s3 = integerSequence_ f3 n
        m1_ = toFloat m1
        m2_ = toFloat m2
        m3_ = toFloat m3
    in
        List.map3 (\x1 x2 x3 -> (toFloat x1)/m1_ + (toFloat x2)/m2_ + (toFloat x3)/m3_) s1 s2 s3
         |> List.map mod1


mod1 x =
    x - (toFloat <| truncate <| x)

m0 = 214748364
m1= 30269
m2= 30307
m3= 30323

a0 = 16807
a1 = 171
a2 = 172
a3 = 170

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

f0 : Maybe Int -> Int
f0 maybeInt =
    case maybeInt of
        Nothing -> m0//2 + 1
        Just k -> modBy m0 (a0*k)

f1 : Maybe Int -> Int
f1 maybeInt =
    case maybeInt of
        Nothing -> m1//2 + 1
        Just k -> modBy m1 (a1*k)


f2 : Maybe Int -> Int
f2 maybeInt =
    case maybeInt of
        Nothing -> m2//2 + 1
        Just k -> modBy m2 (a2*k)


f3 : Maybe Int -> Int
f3 maybeInt =
    case maybeInt of
        Nothing -> m3//2 + 1
        Just k -> modBy m3 (a3*k)

roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)

