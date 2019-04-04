module Cards exposing (..)

import List.Extra

cut : Int -> List a -> List a
cut j list =
   let
       k = modBy (List.length list) j
       (a, b) = List.Extra.splitAt j list
    in
      b ++ a

shuffle : List a -> List a
shuffle list =
    let
      (a, b) = List.Extra.splitAt ((List.length list)//2) list
    in
      List.Extra.interweave a b

dealersMove : Int -> List a -> List a
dealersMove k list =
    list
      |> cut k
      |> shuffle

{-|
  > randomizeList [2, 4, 3] [1,2,3,4,5,6]
  [4,5,3,6,1,2]
-}
randomizeList : (List Int) -> List a -> List a
randomizeList integerList list =
    List.foldl dealersMove list integerList
