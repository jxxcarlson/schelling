module Schelling exposing (..)



{- (
   Cell
   , nRows
   , nCols
   , updateCells
   , initialize
   , inputSequence
   , fractionSatisfied
   , renderAsHtml
  )
 -}

import Array exposing (Array)
import Svg exposing(Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing(Html)
import Array.Extra
import List.Extra
import Utility
import Cards
import Colorbrewer.Qualitative


cellSize = 8



--
-- MATRIX STUFF
--

nRows =
    10

nCols =
    10


{- I am using 1D Arrays, so location
and matrixIndex are used to work with
such arrays as if they were matrices.
-}

location :(Int,  Int) -> Int
location (row, col) =
    nRows * row + col

matrixIndex : Int -> (Int, Int)
matrixIndex n =
    (n // nCols, modBy nCols n)

--
-- Cells
--

{-| Cell and Array Cell are the basic data structures.  T -}
type Cell
    = Occupied CellIndex Id Threshold Identity EmotionalState
    | Unoccupied CellIndex Id

type CellIndex = CellIndex Int

type Id = Id Int

type Threshold
    = Threshold Float

type EmotionalState = Satisfied | Unsatisfied | EUndefined

type Identity
    = Red
    | Blue
    | IUndefined

defaultCell = Unoccupied (CellIndex -1) (Id -1)

identity : Cell -> Identity
identity cell =
    case cell of
        Unoccupied _ _ -> IUndefined
        Occupied _ _ _ ident_ _ -> ident_


emotionalState : Cell -> EmotionalState
emotionalState cell =
   case cell of
        Unoccupied _  _ -> EUndefined
        Occupied _ _ _ _ e -> e

updateEmotionalState : EmotionalState -> Cell -> Cell
updateEmotionalState emotionalState_ cell =
    case cell of
        Unoccupied _ _ -> cell
        Occupied (CellIndex j) (Id k) (Threshold t) ident es -> Occupied (CellIndex j) (Id k) (Threshold t) ident emotionalState_

setCellIndex : Int -> Cell -> Cell
setCellIndex j cell =
    case cell of
        Unoccupied _ (Id k) -> Unoccupied (CellIndex j) (Id k)
        Occupied _ (Id k) (Threshold t) ident es -> Occupied (CellIndex j) (Id k) (Threshold t) ident es


threshold : Cell -> Float
threshold cell =
   case cell of
        Unoccupied _ _ -> 0
        Occupied _ _ (Threshold t) _ _-> t

occupied : Cell -> Bool
occupied cell =
    case cell of
        Occupied _ _ _ _ _->
            True

        Unoccupied _ _ ->
            False

id : Cell -> Int
id cell =
    case cell of
        Unoccupied _ (Id k) -> k
        Occupied _ (Id k) _ _ _ -> k

idx : Cell -> Int
idx cell =
    case cell of
        Unoccupied (CellIndex k) _ -> k
        Occupied (CellIndex k) _ _ _ _ -> k




indexTupleOfCell : Cell -> (Int, Int)
indexTupleOfCell cell =
  case cell of
          Unoccupied (CellIndex j) (Id k) -> (j, k)
          Occupied (CellIndex j) (Id k) _ _ _ -> (j, k)


get : (Int, Int) -> Array Cell ->  Cell
get (row, col) array =
    Array.get (location (row, col)) array |> Maybe.withDefault defaultCell

{-| set is only used in swapWithUnoccupiedCell
 where care is taken to properly update (swap) the
 cell indices.  It would be better not to have this
 funtoin.
 -}
set : (Int, Int) -> Cell -> Array Cell -> Array Cell
set (row, col) cell cellArray =
   Array.set (location (row ,col)) cell cellArray


setWithIndex : Int -> Cell -> Array Cell -> Array Cell
setWithIndex j cell cellArray =
   Array.set j cell cellArray


{-| Replace the cell with id equal to the given cell by the given cell -}
replace : Cell -> Array Cell -> Array Cell
replace cell cellArray =
    Array.set (idx cell) cell cellArray



--
-- NEIGHBORS
--

neighborFilter : ( Int, Int ) -> Bool
neighborFilter ( a, b ) =
    a >= 0 && a < nRows && b >= 0 && b < nCols


neighborIndices : (Int, Int) -> List ( Int, Int )
neighborIndices (row, col) =
    [ ( row - 1, col ), ( row + 1, col ), ( row, col - 1 ), ( row, col + 1 ),
       (row - 1, col - 1), (row - 1, col + 1), (row + 1, col - 1), (row + 1, col - 1)]
        |> List.filter neighborFilter


neighbors : (Int, Int) -> Array Cell -> List Cell
neighbors (row, col) cellArray =
    neighborIndices (row, col)
        |> List.map (\( r, c ) -> get (r, c) cellArray)


--
-- EMOTIONAL STATE
--


{-| Compute the next emotional state of the cell at (row, col)

-}
nextEmotionalState : (Int,  Int) -> Array Cell -> EmotionalState
nextEmotionalState (row, col) array =
    let
        nbs =  neighbors (row, col) array
        numberOfNeighbors = List.length nbs |> toFloat
        me = get (row, col) array
        myThreshold = threshold me
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
        ratioOfLikeMe = sizeOfMyTribe / numberOfNeighbors
    in
       case ratioOfLikeMe < myThreshold  of
           True -> Unsatisfied
           False -> Satisfied

{-| Compute the fraction of occupied cells that have the same
identity as the cell with given (row, col).
-}
fractionLikeMe : (Int, Int) -> Array Cell -> Float
fractionLikeMe (row, col) array =
    let
        nbs =  neighbors (row, col) array
        numberOfNeighbors = List.length nbs |> toFloat
        me = get (row, col) array
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
     in
        sizeOfMyTribe / numberOfNeighbors

listFractionLikeMe : Array Cell -> List Float
listFractionLikeMe cellArray =
    let
      n = Array.length cellArray
      indices = List.range 0 (n - 1)
      tuples = List.map matrixIndex indices
    in
      List.map (\tuple -> fractionLikeMe tuple cellArray) tuples

--
-- UPDATE
--



updateCells : List (Int, Int) ->  Array Cell -> Array Cell
updateCells tupleList cellArray =
    List.foldl updateCell cellArray tupleList


updateCell : (Int, Int) -> Array Cell -> Array Cell
updateCell (i, rand) cellArray =
    let
        (row, col) = matrixIndex i
        updatedCell = updateEmotionalStateOfCellAtIndex  (row, col) cellArray
    in
      if emotionalState updatedCell == Unsatisfied then
        swapWithUnoccupiedCell rand updatedCell cellArray
      else
        replace updatedCell cellArray



updateEmotionalStateOfCellAtIndex : (Int, Int) -> Array Cell -> Cell
updateEmotionalStateOfCellAtIndex  (row, col) cellArray =
   let
       nextEmotionalState_ = nextEmotionalState (row, col) cellArray
       currentCell = Array.get (location (row, col)) cellArray |> Maybe.withDefault defaultCell

   in
      updateEmotionalState nextEmotionalState_ currentCell


swapWithUnoccupiedCell : Int -> Cell -> Array Cell -> Array Cell
swapWithUnoccupiedCell randomNumber cell cellArray =
    let
       unoccupiedSites = cellArray
          |> Array.filter (\cell_ -> not <| occupied cell_)

       i = modBy (Array.length unoccupiedSites) randomNumber
       unoccupiedCell= Array.get i unoccupiedSites |> Maybe.withDefault defaultCell
     in
     swapCells cell unoccupiedCell cellArray

swapCells : Cell -> Cell -> Array Cell -> Array Cell
swapCells cell1 cell2 cellArray =
    let
       idx1 = idx cell1
       idx2 = idx cell2
    in
    cellArray
      |> setWithIndex idx2 (setCellIndex idx2 cell1)
      |> setWithIndex idx1 (setCellIndex idx1 cell2)



{-| This function constructs a list of tuples
  form a list of integers (intended to be
  randomly generated).  It is used as the first
  argument to the updateCells function. -}
inputSequence : Int -> List Int -> List (Int, Int)
inputSequence n rands =
    let
        rands1 = Utility.pad n rands
        rands2 = List.take (max 3 (n//10)) rands |> List.map (\k -> modBy n k)
        randomIndices = Cards.randomize rands2 (List.range 0 (n - 1))
    in
      List.map2 Tuple.pair randomIndices rands1


--
-- MEASURES
--
--
--numberOccupied : Array Cell -> Int
--numberOccupied cellArray =
--   cellArray
--     |> Array.filter (\cell -> occupied cell)
--     |> Array.length

-- Folkert's optimization, see
-- https://gist.github.com/jxxcarlson/2b028fe107e0668d30a15b560d7ce3c3

numberOccupied : Array Cell -> Int
numberOccupied cellArray =
    let folder cell count =
            if occupied cell then
                1 + count
            else
                count
    in
        Array.foldl folder 0 cellArray

--fractionSatisfied : Array Cell -> Float
--fractionSatisfied cellArray =
--    let
--       nOccupied = numberOccupied cellArray |> toFloat
--    in
--   cellArray
--    |> Array.filter  (\cell -> emotionalState cell == Satisfied)
--    |> Array.length
--    |> (\n -> (toFloat n)/nOccupied)

--Folkert's optimization, see
-- https://gist.github.com/jxxcarlson/2b028fe107e0668d30a15b560d7ce3c3
fractionSatisfied : Array Cell -> Float
fractionSatisfied cellArray =
    let
      folder : Cell -> (Int, Int) -> (Int, Int)
      folder cell (occupied2, satisfied) =
            case cell of
                Unoccupied _ _ ->
                    -- no changes
                    ( occupied2, satisfied )

                Occupied _ _ _ _ emotion ->
                    ( occupied2 + 1
                    , if emotion == Satisfied then satisfied + 1 else satisfied
                    )
    in
        Array.foldl folder (0, 0) cellArray
            |> (\(occupied3, satisfied) -> toFloat satisfied / toFloat occupied3)

-- CONSTRUCTORS
--



{-| Construct an array of Cells with given parameters -}
initialize : Float -> Float -> Float -> List Float -> Array Cell
initialize threshold_ probabilityOfUnoccupied probabilityOfRed randomNumbers =
    let
       n = nRows*nCols
       idList = List.range 0 (n-1)
       idList2 = List.map2 Tuple.pair idList idList
       rands = Utility.pad (2*n) randomNumbers
       (a,b) = List.Extra.splitAt n rands
       randTuples = List.map2 Tuple.pair a b
    in
    List.map2 (cellFromTuple threshold_ probabilityOfUnoccupied probabilityOfRed) idList2 randTuples
      |> Array.fromList



{-| Construct a cell from given data -}
cellFromTuple : Float -> Float -> Float -> (Int,Int) -> (Float, Float) -> Cell
cellFromTuple threshold_ probabilityOfUnoccupied probabilityOfRed (index_, id_) (pU,pR) =
    if pU <= probabilityOfUnoccupied then
      Unoccupied (CellIndex index_) (Id id_)
    else if pR  <= probabilityOfRed then
      Occupied (CellIndex index_) (Id id_) (Threshold threshold_) Red Satisfied
    else
      Occupied (CellIndex index_) (Id id_) (Threshold threshold_) Blue Satisfied





--
-- UTILITY
--


{-| Diff two arrays of cells
-}
diff : Array Cell -> Array Cell -> Array (Cell, Cell)
diff cells1 cells2 =
    Array.Extra.zip cells1 cells2
      |> Array.filter (\(a, b) -> a /= b)


--
--  RENDER A CELL ARRAY AS SVG
--


renderAsHtml : Array Cell -> Html msg
renderAsHtml cellArray =
    svg
        [  SA.height <| String.fromFloat 400
           , SA.width <| String.fromFloat 400
        , SA.viewBox <| "0 0 400 400"
        ]
        [ renderAsSvg cellArray]

renderAsSvg : Array Cell -> Svg msg
renderAsSvg cellArray =
    let
        lastCellIndex = (nCols * nRows) - 1
     in
       List.range 0 lastCellIndex
         |> List.map matrixIndex
         |> List.map (renderCell cellArray)
         |> g []


renderCell : Array Cell -> (Int, Int)  -> Svg msg
renderCell cellArray (row, col)  =
    let
         color = case get (row, col) cellArray |> identity of
             Red -> "rgb(166 206 227)"
             Blue -> "rgb(31, 120, 180)"
             IUndefined -> "rgb(40, 40, 40)"
     in
       gridRect cellSize color (row, col)

gridRect : Float -> String -> (Int, Int)  -> Svg msg
gridRect size color (row, col) =
    rect
        [ SA.width <| String.fromFloat size
        , SA.height <| String.fromFloat size
        , SA.x <| String.fromFloat <| size*(toFloat col)
        , SA.y <| String.fromFloat <| size*(toFloat row)
        , SA.fill color
        , SA.strokeWidth "1"
        , SA.stroke "rgb(25, 55, 125)"
        ]
        []

{-


"rgb(166 206 227)"    -- paired3_0 -- light blue
"rgb(31, 120, 180)"  -- paired3_1 -- dark blue
"rgb(178, 223, 138)"  -- paired_3_1 -- light green



-}
