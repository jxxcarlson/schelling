module Schelling exposing (..)

import Array exposing (Array)
import Svg exposing(Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing(Html)
import Array.Extra
import List.Extra
import Utility


nRows =
    32


nCols =
   32

cellSize = 15

modulus = 104729

modelThreshold = 0.6

pUnoccupied = 0.4

cells = initialize modelThreshold pUnoccupied 0.5 (orbit ff (2*nRows*nCols) 23)

type Cell
    = Occupied Id Threshold Identity EmotionalState
    | Unoccupied Id


type Id = Id Int

type Threshold
    = Threshold Float

type EmotionalState = Satisfied | Unsatisfied | EUndefined

type Identity
    = Red
    | Blue
    | IUndefined


identity : Cell -> Identity
identity cell =
    case cell of
        Unoccupied _ -> IUndefined
        Occupied _ _ ident_ _ -> ident_

emotionalState : Cell -> EmotionalState
emotionalState cell =
   case cell of
        Unoccupied _ -> EUndefined
        Occupied _ _ _ e -> e

updateEmotionalState : EmotionalState -> Cell -> Cell
updateEmotionalState emotionalState_ cell =
    case cell of
        Unoccupied _ -> cell
        Occupied (Id k) (Threshold t) ident es -> Occupied (Id k) (Threshold t) ident emotionalState_

threshold : Cell -> Float
threshold cell =
   case cell of
        Unoccupied _ -> 0
        Occupied _ (Threshold t) _ _-> t

occupied : Cell -> Bool
occupied cell =
    case cell of
        Occupied _ _ _ _->
            True

        Unoccupied _ ->
            False

id : Cell -> Int
id cell =
    case cell of
        Unoccupied (Id k) -> k
        Occupied (Id k) _ _ _ -> k


{-| Return index of a cell in an array of cells,  Returns -1 if the cell is not there-}
index : Cell -> Array Cell -> Int
index cell cellArray =
    let
      (status, idx) = Array.foldl (indexAux cell) (False, -1) cellArray
    in
      if status == True then
        idx
      else
         -1




indexAux : Cell -> Cell -> (Bool, Int) -> (Bool, Int)
indexAux givenCell cell state =
    if (id givenCell) == (id cell) then
      (True, (Tuple.second state) + 1)
    else if Tuple.first state == False then
      (False, (Tuple.second state) + 1)
    else
      state

--
-- type CellMatrix
--     = Int Int (Array Cell)


c0 =
    Unoccupied (Id 0)

c0b = Occupied (Id 0) (Threshold 0.3) Red Satisfied

c1 =
    Occupied (Id 1) (Threshold 0.3) Red Satisfied


c2=
    Unoccupied (Id 2)


c3 =
    Occupied (Id 3) (Threshold 0.3) Blue Satisfied


c4 =
    Occupied (Id 4) (Threshold 0.3) Blue Satisfied


c5 =
    Unoccupied (Id 5)


c6 =
    Occupied (Id 6) (Threshold 0.3) Blue Satisfied


c7 =
    Unoccupied (Id 7)


c8 =
    Occupied (Id 8) (Threshold 0.4) Blue Satisfied

c8b =
    Occupied (Id 8) (Threshold 0.4) Red Satisfied

c9 = Unoccupied (Id 9)

testCells =
    Array.fromList [ c0, c1, c2, c3, c4, c5, c6, c7, c8 ]


testSequence n =
    List.map2 Tuple.pair (List.range 0 (n-1)) (List.range 0 (n-1))


inputSequence : Int -> List Int -> List (Int, Int)
inputSequence n rands =
    let
        rands1 = pad n rands
        rands2 = List.take (max 3 (n//10)) rands |> List.map (\k -> modBy n k) |> Debug.log "rands2"
        randomIndices = randomizeList rands2 (List.range 0 (n - 1))
    in
      List.map2 Tuple.pair randomIndices rands1


sameIdentity : Cell -> Cell -> Bool
sameIdentity a b =
        identity a == identity b




location : Int -> Int -> Int
location row col =
    nRows * row + col

indexTuple : Int -> (Int, Int)
indexTuple n =
    (n // nCols, modBy nCols n)

indexTupleOfCell : Cell -> Array Cell -> (Int, Int)
indexTupleOfCell cell cellArray =
   index cell cellArray |> indexTuple

get : (Int, Int) -> Array Cell ->  Cell
get (row, col) array =
    Array.get (location row col) array |> Maybe.withDefault (Unoccupied <| Id -1)


--
-- EMOTIONAL STATE
--

neighborFilter : ( Int, Int ) -> Bool
neighborFilter ( a, b ) =
    a >= 0 && a < nRows && b >= 0 && b < nCols


neighborIndices : Int -> Int -> List ( Int, Int )
neighborIndices row col =
    [ ( row - 1, col ), ( row + 1, col ), ( row, col - 1 ), ( row, col + 1 ) ]
        |> List.filter neighborFilter


neighbors : Int -> Int -> Array Cell -> List Cell
neighbors row col cellArray =
    neighborIndices row col
        |> List.map (\( r, c ) -> get (r, c) cellArray)

{-| Compute the next emotional state of the cell at (row, col)

-}
nextEmotionalState : (Int,  Int) -> Array Cell -> EmotionalState
nextEmotionalState (row, col) array =
    let
        nbs =  neighbors row col array
        numberOfNeihbors = List.length nbs |> toFloat
        me = get (row, col) array
        myThreshold = threshold me
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
        ratio_ = sizeOfMyTribe / numberOfNeihbors
    in
       case ratio_ < myThreshold  of
           True -> Unsatisfied
           False -> Satisfied


ratio : (Int, Int) -> Array Cell -> Float
ratio (row, col) array =
    let
        nbs =  neighbors row col array
        numberOfNeighbors = List.length nbs |> toFloat
        me = get (row, col) array
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
     in
        sizeOfMyTribe / numberOfNeighbors

--
-- UPDATE
--

set : (Int, Int) -> Cell -> Array Cell -> Array Cell
set (row, col) cell cellArray =
   Array.set (location row col) cell cellArray

{-| Replace the cell with id equal to the given cell by the given cell -}
replace : Cell -> Array Cell -> Array Cell
replace cell cellArray =
    Array.set (index cell cellArray) cell cellArray



updateEmotionalStateAtIndex : Int -> Int -> Array Cell -> Array Cell
updateEmotionalStateAtIndex  row col cellArray =
   let
       nextEmotionalState_ = nextEmotionalState (row, col) cellArray
       currentCell = Array.get (location row col) cellArray |> Maybe.withDefault (Unoccupied (Id -1))
       nextCell = updateEmotionalState nextEmotionalState_ currentCell
   in
      replace nextCell cellArray

updateEmotionalStateOfCellAtIndex : Int -> Int -> Array Cell -> Cell
updateEmotionalStateOfCellAtIndex  row col cellArray =
   let
       nextEmotionalState_ = nextEmotionalState (row, col) cellArray
       currentCell = Array.get (location row col) cellArray |> Maybe.withDefault (Unoccupied (Id -1))

   in
      updateEmotionalState nextEmotionalState_ currentCell


swapWithUnoccupiedCell : Int -> Cell -> Array Cell -> Array Cell
swapWithUnoccupiedCell randomNumber cell cellArray =
    let
       unoccupiedSites = cellArray
          |> Array.filter (\cell_ -> not <| occupied cell_)

       i = modBy (Array.length unoccupiedSites) randomNumber
       unoccupiedSite = Array.get i unoccupiedSites |> Maybe.withDefault (Unoccupied (Id -1))
       idxTupleCell = indexTupleOfCell cell cellArray
       idxUnoccupied = indexTupleOfCell unoccupiedSite cellArray
     in
     cellArray
       |> set idxUnoccupied cell
       |> set idxTupleCell unoccupiedSite


updateCells : List (Int, Int) ->  Array Cell -> Array Cell
updateCells tupleList cellArray =
    List.foldl updateCell cellArray tupleList


updateCell : (Int, Int) -> Array Cell -> Array Cell
updateCell (idx, rand) cellArray =
    let
        (row, col) = indexTuple idx
        updatedCell = updateEmotionalStateOfCellAtIndex  row col cellArray
    in
      if emotionalState updatedCell == Unsatisfied then
        swapWithUnoccupiedCell rand updatedCell cellArray
      else
        replace updatedCell cellArray


cutList : Int -> List a -> List a
cutList k list =
   let
       kk = modBy (List.length list) k
       (a, b) = List.Extra.splitAt kk list
    in
      b ++ a


shuffle : List a -> List a
shuffle list =
    let
      (firstHalf, secondHalf) = List.Extra.splitAt ((List.length list)//2) list
    in
      List.Extra.interweave firstHalf secondHalf

dealersMove : Int -> List a -> List a
dealersMove k list =
    list
      |> cutList k
      |> shuffle



{-|
  > randomizeList [2, 4, 3] [1,2,3,4,5,6]
  [4,5,3,6,1,2]
-}
randomizeList : (List Int) -> List a -> List a
randomizeList randInts list =
    List.foldl dealersMove list randInts

--
-- MEASURES
--

numberOccupied : Array Cell -> Int
numberOccupied cellArray =
   cellArray
     |> Array.filter (\cell -> occupied cell)
     |> Array.length

fractionSatisfied : Array Cell -> Float
fractionSatisfied cellArray =
    let
       nOccupied = numberOccupied cellArray |> toFloat
    in
   cellArray
    |> Array.filter  (\cell -> emotionalState cell == Satisfied)
    |> Array.length
    |> (\n -> (toFloat n)/nOccupied)
--
-- INITIALIZATION
--

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


cellFromTuple : Float -> Float -> Float -> (Int, (Float, Float)) -> Cell
cellFromTuple threshold_ probabilityOfUnoccupied probabilityOfRed (id_, (pU,pR)) =
    if pU <= probabilityOfUnoccupied then
      Unoccupied (Id id_)
    else if pR  <= probabilityOfRed then
      Occupied (Id id_) (Threshold threshold_) Red Satisfied
    else
      Occupied (Id id_) (Threshold threshold_) Blue Satisfied

initialize : Float -> Float -> Float -> List Float -> Array Cell
initialize threshold_ probabilityOfUnoccupied probabilityOfRed randomNumbers =
    let
       n = nRows*nCols
       idList = List.range 0 (n-1)
       rands = pad (2*n) randomNumbers
       (a,b) = List.Extra.splitAt n rands
       randTuples = List.map2 Tuple.pair a b
       tupleList = List.map2 Tuple.pair idList randTuples
    in
    List.map (cellFromTuple threshold_ probabilityOfUnoccupied probabilityOfRed) tupleList
      |> Array.fromList



orbit : (Maybe Int -> Int) -> Int ->  Int -> List Float
orbit f n seed  =
    orbitAux f (n+1) [seed]
      |> List.map (\k -> Utility.roundTo 4 <| (toFloat k)/modulus)
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

--
-- UTILITY
--


diff : Array Cell -> Array Cell -> Array (Cell, Cell)
diff cells1 cells2 =
    Array.Extra.zip cells1 cells2
      |> Array.filter (\(a, b) -> a /= b)


--
-- VISUALIZAtion
--


renderAsHtml : Array Cell -> Html msg
renderAsHtml cellArray =
    svg
        [  SA.height <| String.fromFloat 600
           , SA.width <| String.fromFloat 600
        , SA.viewBox <| "0 0 600 600"
        ]
        [ renderAsSvg cellArray]

renderAsSvg : Array Cell -> Svg msg
renderAsSvg cellArray =
    let
        lastCellIndex = (nCols * nRows) - 1
     in
       List.range 0 lastCellIndex
         |> List.map indexTuple
         |> List.map (renderCell cellArray)
         |> g []


renderCell : Array Cell -> (Int, Int)  -> Svg msg
renderCell cellArray (row, col)  =
    let
         color = case get (row, col) cellArray |> identity of
             Red -> "red"
             Blue -> "blue"
             IUndefined -> "black"
     in
       gridRect cellSize color row col

gridRect : Float -> String -> Int -> Int  -> Svg msg
gridRect size color row col =
    rect
        [ SA.width <| String.fromFloat size
        , SA.height <| String.fromFloat size
        , SA.x <| String.fromFloat <| size*(toFloat col)
        , SA.y <| String.fromFloat <| size*(toFloat row)
        , SA.fill color
        ]
        []