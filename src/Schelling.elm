module Schelling exposing (..)

import Array exposing (Array)
import Svg exposing(Svg, svg, rect, g)
import Svg.Attributes as SA
import Html exposing(Html)


type Cell
    = Occupied Threshold Identity EmotionalState
    | Unoccupied


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
        Unoccupied -> IUndefined
        Occupied _ ident_ _ -> ident_

emotionalState : Cell -> EmotionalState
emotionalState cell =
   case cell of
        Unoccupied -> EUndefined
        Occupied _ _ e -> e

threshold : Cell -> Float
threshold cell =
   case cell of
        Unoccupied -> 0
        Occupied (Threshold t) _ _-> t
--
-- type CellMatrix
--     = Int Int (Array Cell)


c1 =
    Unoccupied


c2 =
    Occupied (Threshold 0.3) Red Satisfied


c3 =
    Unoccupied


c4 =
    Occupied (Threshold 0.3) Blue Satisfied


c5 =
    Occupied (Threshold 0.3) Blue Satisfied


c6 =
    Unoccupied


c7 =
    Occupied (Threshold 0.3) Blue Satisfied


c8 =
    Unoccupied


c9 =
    Occupied (Threshold 0.4) Blue Satisfied

cells =
    Array.fromList [ c1, c2, c3, c4, c5, c6, c7, c8, c9 ]


nRows =
    3


nCols =
    3

cellSize = 30

occupied : Cell -> Bool
occupied cell =
    case cell of
        Occupied _ _ _->
            True

        Unoccupied ->
            False


sameIdentity : Cell -> Cell -> Bool
sameIdentity a b =
        identity a == identity b




location : Int -> Int -> Int
location row col =
    nRows * row + col

index : Int -> (Int, Int)
index n =
    (modBy nCols n, n // nCols)

getElement : Int -> Int -> Array Cell ->  Cell
getElement row col array =
    Array.get (location row col) array |> Maybe.withDefault Unoccupied




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
        |> List.map (\( r, c ) -> getElement r c cellArray)


nextEmotionalState : Int -> Int -> Array Cell -> EmotionalState
nextEmotionalState row col array =
    let
        nbs =  neighbors row col array
        numberOfNeihbors = List.length nbs |> toFloat
        me = getElement row col array
        myThreshold = threshold me
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
        ratio_ = sizeOfMyTribe / numberOfNeihbors
    in
       case ratio_ > myThreshold  of
           True -> Satisfied
           False -> Unsatisfied


ratio : Int -> Int -> Array Cell -> Float
ratio row col array =
    let
        nbs =  neighbors row col array
        numberOfNeihbors = List.length nbs |> toFloat
        me = getElement row col array
        myThreshold = threshold me
        myIdentity = identity me
        myTribe = nbs |> List.filter (\cell -> identity cell == myIdentity)
        sizeOfMyTribe = toFloat (List.length myTribe)
     in
        sizeOfMyTribe / numberOfNeihbors


--
-- VISUALIZAtion
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
         |> List.map index
         |> List.map (renderCell cellArray)
         |> g []


renderCell : Array Cell -> (Int, Int)  -> Svg msg
renderCell cellArray (row, col)  =
    let
         color = case getElement row col cellArray |> identity of
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