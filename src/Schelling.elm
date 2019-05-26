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
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing (Html)
import Array.Extra
import List.Extra
import Utility
import Cards
import Colorbrewer.Qualitative


type InitialState
    = Random
    | Stable
    | Unstable Int


type alias Model =
    { nRows : Int
    , nCols : Int
    , threshold : Float
    , probabilityOfUnoccupied : Float
    , probabilityOfRed : Float
    , cellSize : Float
    }


defaultModel =
    { nRows = 40
    , nCols = 40
    , threshold = 0.375
    , probabilityOfUnoccupied = 0.1
    , probabilityOfRed = 0.5
    , cellSize = 8
    }



{- I am using 1D Arrays, so location
   and matrixIndex are used to work with
   such arrays as if they were matrices.
-}


location : Model -> ( Int, Int ) -> Int
location model ( row, col ) =
    model.nRows * row + col


matrixIndex : Model -> Int -> ( Int, Int )
matrixIndex model n =
    ( n // model.nCols, modBy model.nCols n )



--
-- Cells
--


{-| Cell and Array Cell are the basic data structures. T
-}
type Cell
    = Occupied CellIndex Id Threshold Identity EmotionalState
    | Unoccupied CellIndex Id


type CellIndex
    = CellIndex Int


type Id
    = Id Int


type Threshold
    = Threshold Float


type EmotionalState
    = Satisfied
    | Unsatisfied
    | EUndefined


type Identity
    = Red
    | Blue
    | IUndefined


defaultCell =
    Unoccupied (CellIndex -1) (Id -1)


identity : Cell -> Identity
identity cell =
    case cell of
        Unoccupied _ _ ->
            IUndefined

        Occupied _ _ _ ident_ _ ->
            ident_


emotionalState : Cell -> EmotionalState
emotionalState cell =
    case cell of
        Unoccupied _ _ ->
            EUndefined

        Occupied _ _ _ _ e ->
            e


updateEmotionalState : EmotionalState -> Cell -> Cell
updateEmotionalState emotionalState_ cell =
    case cell of
        Unoccupied _ _ ->
            cell

        Occupied (CellIndex j) (Id k) (Threshold t) ident es ->
            Occupied (CellIndex j) (Id k) (Threshold t) ident emotionalState_


setCellIndex : Int -> Cell -> Cell
setCellIndex j cell =
    case cell of
        Unoccupied _ (Id k) ->
            Unoccupied (CellIndex j) (Id k)

        Occupied _ (Id k) (Threshold t) ident es ->
            Occupied (CellIndex j) (Id k) (Threshold t) ident es


threshold : Cell -> Float
threshold cell =
    case cell of
        Unoccupied _ _ ->
            0

        Occupied _ _ (Threshold t) _ _ ->
            t


occupied : Cell -> Bool
occupied cell =
    case cell of
        Occupied _ _ _ _ _ ->
            True

        Unoccupied _ _ ->
            False


id : Cell -> Int
id cell =
    case cell of
        Unoccupied _ (Id k) ->
            k

        Occupied _ (Id k) _ _ _ ->
            k


idx : Cell -> Int
idx cell =
    case cell of
        Unoccupied (CellIndex k) _ ->
            k

        Occupied (CellIndex k) _ _ _ _ ->
            k


indexTupleOfCell : Cell -> ( Int, Int )
indexTupleOfCell cell =
    case cell of
        Unoccupied (CellIndex j) (Id k) ->
            ( j, k )

        Occupied (CellIndex j) (Id k) _ _ _ ->
            ( j, k )


get : Model -> ( Int, Int ) -> Array Cell -> Cell
get model ( row, col ) array =
    Array.get (location model ( row, col )) array |> Maybe.withDefault defaultCell


{-| set is only used in swapWithUnoccupiedCell
where care is taken to properly update (swap) the
cell indices. It would be better not to have this
funtoin.
-}
set : Model -> ( Int, Int ) -> Cell -> Array Cell -> Array Cell
set model ( row, col ) cell cellArray =
    Array.set (location model ( row, col )) cell cellArray


setWithIndex : Int -> Cell -> Array Cell -> Array Cell
setWithIndex j cell cellArray =
    Array.set j cell cellArray


{-| Replace the cell with id equal to the given cell by the given cell
-}
replace : Cell -> Array Cell -> Array Cell
replace cell cellArray =
    Array.set (idx cell) cell cellArray



--
-- NEIGHBORS
--


neighborFilter : Model -> ( Int, Int ) -> Bool
neighborFilter model ( a, b ) =
    a >= 0 && a < model.nRows && b >= 0 && b < model.nCols


neighborIndices : Model -> ( Int, Int ) -> List ( Int, Int )
neighborIndices model ( row, col ) =
    [ ( row - 1, col )
    , ( row + 1, col )
    , ( row, col - 1 )
    , ( row, col + 1 )
    , ( row - 1, col - 1 )
    , ( row - 1, col + 1 )
    , ( row + 1, col - 1 )
    , ( row + 1, col - 1 )
    ]
        |> List.filter (neighborFilter model)


neighbors : Model -> ( Int, Int ) -> Array Cell -> List Cell
neighbors model ( row, col ) cellArray =
    neighborIndices model ( row, col )
        |> List.map (\( r, c ) -> get model ( r, c ) cellArray)



--
-- EMOTIONAL STATE
--


{-| Compute the next emotional state of the cell at (row, col)
-}
nextEmotionalState : Model -> ( Int, Int ) -> Array Cell -> EmotionalState
nextEmotionalState model ( row, col ) array =
    let
        nbs =
            neighbors model ( row, col ) array

        numberOfNeighbors =
            List.length nbs |> toFloat

        me =
            get model ( row, col ) array

        myThreshold =
            threshold me

        myIdentity =
            identity me

        myTribe =
            nbs |> List.filter (\cell -> identity cell == myIdentity)

        sizeOfMyTribe =
            toFloat (List.length myTribe)

        ratioOfLikeMe =
            sizeOfMyTribe / numberOfNeighbors
    in
        case ratioOfLikeMe < myThreshold of
            True ->
                Unsatisfied

            False ->
                Satisfied


{-| Compute the fraction of occupied cells that have the same
identity as the cell with given (row, col).
-}
fractionLikeMe : Model -> ( Int, Int ) -> Array Cell -> Float
fractionLikeMe model ( row, col ) array =
    let
        nbs =
            neighbors model ( row, col ) array

        numberOfNeighbors =
            List.length nbs |> toFloat

        me =
            get model ( row, col ) array

        myIdentity =
            identity me

        myTribe =
            nbs |> List.filter (\cell -> identity cell == myIdentity)

        sizeOfMyTribe =
            toFloat (List.length myTribe)
    in
        sizeOfMyTribe / numberOfNeighbors


listFractionLikeMe : Model -> Array Cell -> List Float
listFractionLikeMe model cellArray =
    let
        n =
            Array.length cellArray

        indices =
            List.range 0 (n - 1)

        tuples =
            List.map (matrixIndex model) indices
    in
        List.map (\tuple -> fractionLikeMe model tuple cellArray) tuples


aggregateFractionLikeMe : Model -> Array Cell -> Float
aggregateFractionLikeMe model cellArray =
    let
        ratios = listFractionLikeMe model cellArray
        sumOfRatios = List.sum ratios
        n = List.length ratios  |> toFloat
    in
       sumOfRatios/n


--
-- UPDATE
--


updateCells : Model -> List ( Int, Int ) -> Array Cell -> Array Cell
updateCells model tupleList cellArray =
    List.foldl (updateCell model) cellArray tupleList


updateCell : Model -> ( Int, Int ) -> Array Cell -> Array Cell
updateCell model ( i, rand ) cellArray =
    let
        ( row, col ) =
            matrixIndex model i

        updatedCell =
            updateEmotionalStateOfCellAtIndex model ( row, col ) cellArray
    in
        if emotionalState updatedCell == Unsatisfied then
            swapWithUnoccupiedCell rand updatedCell cellArray
        else
            replace updatedCell cellArray


updateEmotionalStateOfCellAtIndex : Model -> ( Int, Int ) -> Array Cell -> Cell
updateEmotionalStateOfCellAtIndex model ( row, col ) cellArray =
    let
        nextEmotionalState_ =
            nextEmotionalState model ( row, col ) cellArray

        currentCell =
            Array.get (location model ( row, col )) cellArray |> Maybe.withDefault defaultCell
    in
        updateEmotionalState nextEmotionalState_ currentCell


swapWithUnoccupiedCell : Int -> Cell -> Array Cell -> Array Cell
swapWithUnoccupiedCell randomNumber cell cellArray =
    let
        unoccupiedSites =
            cellArray
                |> Array.filter (\cell_ -> not <| occupied cell_)

        i =
            modBy (Array.length unoccupiedSites) randomNumber

        unoccupiedCell =
            Array.get i unoccupiedSites |> Maybe.withDefault defaultCell
    in
        swapCells cell unoccupiedCell cellArray


swapCells : Cell -> Cell -> Array Cell -> Array Cell
swapCells cell1 cell2 cellArray =
    let
        idx1 =
            idx cell1

        idx2 =
            idx cell2
    in
        cellArray
            |> setWithIndex idx2 (setCellIndex idx2 cell1)
            |> setWithIndex idx1 (setCellIndex idx1 cell2)


{-| This function constructs a list of tuples
form a list of integers (intended to be
randomly generated). It is used as the first
argument to the updateCells function.
-}
inputSequence : Model -> List Int -> List ( Int, Int )
inputSequence model rands =
    let
        n =
            model.nRows * model.nCols

        rands1 =
            Utility.pad n rands

        rands2 =
            List.take (max 3 (n // 10)) rands |> List.map (\k -> modBy n k)

        randomIndices =
            Cards.randomize rands2 (List.range 0 (n - 1))
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
    let
        folder cell count =
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
        folder : Cell -> ( Int, Int ) -> ( Int, Int )
        folder cell ( occupied2, satisfied ) =
            case cell of
                Unoccupied _ _ ->
                    -- no changes
                    ( occupied2, satisfied )

                Occupied _ _ _ _ emotion ->
                    ( occupied2 + 1
                    , if emotion == Satisfied then
                        satisfied + 1
                      else
                        satisfied
                    )
    in
        Array.foldl folder ( 0, 0 ) cellArray
            |> (\( occupied3, satisfied ) -> toFloat satisfied / toFloat occupied3)



-- CONSTRUCTORS
--


{-| Construct an array of Cells with given parameters
-}
initialize : Model -> List Float -> Array Cell
initialize model randomNumbers =
    let
        n =
            model.nRows * model.nCols

        idList =
            List.range 0 (n - 1)

        idList2 =
            List.map2 Tuple.pair idList idList

        rands =
            Utility.pad (2 * n) randomNumbers

        ( a, b ) =
            List.Extra.splitAt n rands

        randTuples =
            List.map2 Tuple.pair a b
    in
        List.map2 (cellFromTuple model) idList2 randTuples
            |> Array.fromList


{-| Construct a cell from given data
-}
cellFromTuple : Model -> ( Int, Int ) -> ( Float, Float ) -> Cell
cellFromTuple model ( index_, id_ ) ( pU, pR ) =
    if pU <= model.probabilityOfUnoccupied then
        Unoccupied (CellIndex index_) (Id id_)
    else if pR <= model.probabilityOfRed then
        Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Red Satisfied
    else
        Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Blue Satisfied


initialize2 : InitialState -> Model -> Int -> Array Cell
initialize2 initialState model limit =
    let
        cellBuilder =
            case initialState of
                Stable ->
                    cellFromTuple2

                Unstable 1 ->
                    cellFromTuple3

                Unstable 2 ->
                    cellFromTuple4

                _ ->
                    cellFromTuple2
    in
        initializeWithBuilder cellBuilder model limit


initializeWithBuilder : CellBuilder -> Model -> Int -> Array Cell
initializeWithBuilder cellBuilder model limit =
    let
        n =
            model.nRows * model.nCols

        idList =
            List.range 0 (n - 1)

        idList2 =
            List.map2 Tuple.pair idList idList
    in
        List.map (cellBuilder model limit) idList2
            |> Array.fromList


type alias CellBuilder =
    Model -> Int -> ( Int, Int ) -> Cell


cellFromTuple2 : Model -> Int -> ( Int, Int ) -> Cell
cellFromTuple2 model limit ( index_, id_ ) =
    let
        row =
            index_ // model.nCols

        shift =
            2 * (encodeZ4 2 row)

        k =
            encodeZ4 shift index_
    in
        if index_ >= limit then
            Unoccupied (CellIndex index_) (Id id_)
        else if k == 1 then
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Red Satisfied
        else
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Blue Satisfied


cellFromTuple3 : Model -> Int -> ( Int, Int ) -> Cell
cellFromTuple3 model limit ( index_, id_ ) =
    let
        row =
            index_ // model.nCols

        shift =
            2 * (encodeZ4 2 row)

        k =
            encodeZ4 shift index_
    in
        if index_ >= limit then
            Unoccupied (CellIndex index_) (Id id_)
        else if index_ > 20 && index_ < 40 then
            Unoccupied (CellIndex index_) (Id id_)
        else if k == 1 then
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Red Satisfied
        else
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Blue Satisfied


cellFromTuple4 : Model -> Int -> ( Int, Int ) -> Cell
cellFromTuple4 model limit ( index_, id_ ) =
    let
        row =
            index_ // model.nCols

        shift =
            2 * (encodeZ4 2 row)

        k =
            encodeZ4 shift index_
    in
        if index_ >= limit then
            Unoccupied (CellIndex index_) (Id id_)
        else if index_ > 20 && index_ < 40 then
            Unoccupied (CellIndex index_) (Id id_)
        else if modBy 17 (7 * index_) == 1 then
            Unoccupied (CellIndex index_) (Id id_)
        else if k == 1 then
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Red Satisfied
        else
            Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Blue Satisfied


encodeZ4 : Int -> Int -> Int
encodeZ4 shift k =
    case modBy 4 (shift + k) of
        0 ->
            1

        1 ->
            1

        2 ->
            0

        3 ->
            0

        _ ->
            0



-- {-| Construct a cell from given data -}
-- cellFromTuple2 : Model -> (Int,Int)  -> Cell
-- cellFromTuple2 model (index_, id_) =
--
--
-- if pU <= model.probabilityOfUnoccupied then
--   Unoccupied (CellIndex index_) (Id id_)
-- else if pR  <= model.probabilityOfRed then
--   Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Red Satisfied
-- else
--   Occupied (CellIndex index_) (Id id_) (Threshold model.threshold) Blue Satisfied
--
-- UTILITY
--


{-| Diff two arrays of cells
-}
diff : Array Cell -> Array Cell -> Array ( Cell, Cell )
diff cells1 cells2 =
    Array.Extra.zip cells1 cells2
        |> Array.filter (\( a, b ) -> a /= b)



--
--  RENDER A CELL ARRAY AS SVG
--


renderAsHtml : Model -> Array Cell -> Html msg
renderAsHtml model cellArray =
    svg
        [ SA.height <| String.fromFloat 400
        , SA.width <| String.fromFloat 400
        , SA.viewBox <| "0 0 400 400"
        ]
        [ renderAsSvg model cellArray ]


renderAsSvg : Model -> Array Cell -> Svg msg
renderAsSvg model cellArray =
    let
        lastCellIndex =
            (model.nCols * model.nRows) - 1
    in
        List.range 0 lastCellIndex
            |> List.map (matrixIndex model)
            |> List.map (renderCell model cellArray)
            |> g []


renderCell : Model -> Array Cell -> ( Int, Int ) -> Svg msg
renderCell model cellArray ( row, col ) =
    let
        color =
            case get model ( row, col ) cellArray |> identity of
                Red ->
                    "rgb(166 206 227)"

                Blue ->
                    "rgb(31, 120, 180)"

                IUndefined ->
                    "rgb(40, 40, 40)"
    in
        gridRect model.cellSize color ( row, col )


gridRect : Float -> String -> ( Int, Int ) -> Svg msg
gridRect size color ( row, col ) =
    rect
        [ SA.width <| String.fromFloat size
        , SA.height <| String.fromFloat size
        , SA.x <| String.fromFloat <| size * (toFloat col)
        , SA.y <| String.fromFloat <| size * (toFloat row)
        , SA.fill color
        , SA.strokeWidth "1"
        , SA.stroke "rgb(25, 55, 125)"
        ]
        []
