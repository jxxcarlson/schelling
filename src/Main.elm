module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Schelling exposing (Cell, InitialState(..))
import Array exposing (Array)
import Time exposing (Posix)
import Random
import Utility
import Text
import RNG
import Graph


tickInterval =
    500


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , cells : Array Cell
    , schellingModel : Schelling.Model
    , satisfactionData : List (Float, Float)
    , similarityData : List (Float, Float)
    , numberLikeMeString : String
    , fractionUnoccupiedString : String
    , tickCount : Int
    , randomNumber : Float
    , cellIndex : Int
    , appState : AppState
    , initialState : InitialState
    }


type AppState
    = Go
    | Stop


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dm =
            Schelling.defaultModel
    in
        ( { input = "App started"
          , output = "App started"
          , cells = Schelling.initialize Schelling.defaultModel (RNG.floatSequence (2 * dm.nRows * dm.nCols) 23 ( 0, 1 ))
          , schellingModel = dm
          , satisfactionData = []
          , similarityData = []
          , numberLikeMeString = "3"
          , fractionUnoccupiedString = "10"
          , tickCount = 0
          , randomNumber = 0
          , cellIndex = 0
          , appState = Stop
          , initialState = Random
          }
        , Cmd.none
        )



{- MSG -}


type Msg
    = NoOp
    | InputThreshold String
    | InputFractionUnoccupied String
    | UpdateModel
    | Tick Posix
    | NewRandomNumbers (List Int)
    | ToggleAppState
    | Reset
    | SetInitialState InitialState


type alias Flags =
    {}


subscriptions model =
    Time.every tickInterval Tick


stringToFloat : String -> Float -> Float
stringToFloat str default =
    String.toFloat str |> Maybe.withDefault default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputThreshold numberLikeMeString ->
            let
                threshold = (numberLikeMeString |> String.toFloat |> Maybe.withDefault 3) / 8
                sm = model.schellingModel
                smNew = { sm | threshold = threshold }
                newCells = Schelling.setThresholds threshold model.cells
             in
               ( { model | numberLikeMeString = numberLikeMeString, schellingModel = smNew, cells = newCells }, Cmd.none )

        InputFractionUnoccupied str ->
            ( { model | fractionUnoccupiedString = str }, Cmd.none )

        UpdateModel ->
            ( model, Cmd.none )

        Tick posix ->
            case model.appState of
                Stop ->
                    ( model, Cmd.none )

                Go ->
                      ( { model | tickCount = model.tickCount + 1 }, Random.generate NewRandomNumbers (Random.list 1000 (Random.int 0 100000)) )

        NewRandomNumbers randList ->
            let
                sm =
                    model.schellingModel

                n =
                    sm.nRows * sm.nCols - 1
            in
                if model.appState == Stop then
                    ( model, Cmd.none )
                else
                  let
                      newCells = Schelling.updateCells model.schellingModel
                                     (Schelling.inputSequence model.schellingModel randList)
                                      model.cells
                      satisfied = Schelling.fractionSatisfied newCells
                      similar = Schelling.aggregateFractionLikeMe model.schellingModel newCells
                      nextState = if model.tickCount < 100 then model.appState else Stop
                      t = model.tickCount |> toFloat
                  in
                    ( { model
                        | cells = newCells
                          , satisfactionData = (t, satisfied)::model.satisfactionData
                          , similarityData = (t, similar)::model.similarityData
                          , appState = nextState

                      }
                    , Cmd.none
                    )



        ToggleAppState ->
            case model.appState of
                Stop ->
                    ( { model | appState = Go }, Cmd.none )

                Go ->
                    ( { model | appState = Stop }, Cmd.none )

        Reset ->
            case model.initialState of
                Random ->
                    resetRandom model

                Stable ->
                    resetStable model 0

                Unstable k ->
                    resetStable model k

        SetInitialState initialState ->
            ( { model | initialState = initialState, satisfactionData = [], similarityData = [] }, Cmd.none )



-- ( { model | cells = Schelling.initialize2 Schelling.defaultModel 400 }, Cmd.none )


resetStable : Model -> Int -> ( Model, Cmd Msg )
resetStable model k =
    let
        sm =
            model.schellingModel

        n =
            sm.nCols * sm.nRows

        threshold =
            (String.toFloat model.numberLikeMeString |> Maybe.withDefault 3) / 8

        smNew =
            { sm | threshold = threshold }

        newCells =
            Schelling.initialize2 model.initialState smNew (n // 2)
    in
        ( { model
            | cells = newCells
            , schellingModel = smNew
            , tickCount = 0
            , appState = Stop
            , satisfactionData = [], similarityData = []
          }
        , Cmd.none
        )


resetRandom : Model -> ( Model, Cmd Msg )
resetRandom model =
    let
        sm =
            model.schellingModel

        threshold =
            (String.toFloat model.numberLikeMeString |> Maybe.withDefault 3) / 8

        probabilityOfUnoccupied =
            (String.toFloat model.fractionUnoccupiedString |> Maybe.withDefault 10) / 100.0

        smNew =
            { sm | threshold = threshold, probabilityOfUnoccupied = probabilityOfUnoccupied }
    in
        ( { model
            | cells = Schelling.initialize smNew (RNG.floatSequence (2 * smNew.nRows * smNew.nCols) 23 ( 0, 1 ))
            , schellingModel = smNew
            , tickCount = 0
            , appState = Stop
            , satisfactionData = [], similarityData = []
          }
        , Cmd.none
        )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    row [ spacing 24, centerX, centerY ] [ appPanel model, Text.panel ]


appPanel : Model -> Element Msg
appPanel model =
    column mainColumnStyle
        [ title "Schelling model"
        , display model
        , column [moveUp 80, spacing 5] [
              charts model
            , controls model
            , indicators model
            , initialStateSelector model
            ]
        ]



charts model =
    column [spacing 15, paddingEach {top = 0, right = 0, bottom = 20, left = 0}, moveUp 10] [
        row [spacing 30] [
                     Graph.lineChartWithDataWindow  dataWindow lineGraphAttributes model.satisfactionData |> Element.html,
                     Graph.lineChartWithDataWindow  dataWindow lineGraphAttributes model.similarityData |> Element.html
                 ]
       , chartLabels
       ]

chartLabels =
    row [spacing 50, Font.size 11, moveRight 30] [
        el [width (px 200)] (el [centerX] (text "Satisfaction"))
        , el [width (px 200)] (el [centerX] (text "Similarity Index"))
    ]


dataWindow = {
    xMin = 0.0
  , xMax = 100.0
  , yMin = 0.0
  , yMax = 1.0
 }


lineGraphAttributes =
    {   graphHeight = 60
      , graphWidth = 200
      , options = [ Graph.Color "red",Graph.XTickmarks 11, Graph.YTickmarks 5]
    }

initialStateSelector model =
    row [ spacing 12, paddingXY 0 10 ]
        [ randomInitialModelButton model
        , stableInitialModelButton model
        , unstableInitialModelButton1 model
        , unstableInitialModelButton2 model
        ]


display : Model -> Element Msg
display model =
    column [ moveRight 90 ] [ Schelling.renderAsHtml model.schellingModel model.cells |> Element.html ]


controls : Model -> Element Msg
controls model =
    row [ spacing 12, moveUp 12 ] [ goButton model
       , resetButton
       , inputTreshold model
       , inputFractionOccupied model
       , statusIndicator model]


statusIndicator model =
    let
        (bgColor, message)  = case model.appState of
            Go -> (Element.rgb255 0 140 0, "Running")
            Stop -> (Element.rgb255 140 0 0, "Stopped")
    in
      el [Background.color bgColor, Font.size 12, Font.color <| Element.rgb 1 1 1, padding 6] (text message)


indicators : Model -> Element Msg
indicators model =
    row [ spacing 18, width (px 430) ]
        [ el [ Font.size 14, width (px 55) ] (text <| "cycle: " ++ String.fromInt model.tickCount)
        , el [ Font.size 14, width (px numberDisplayWidth) ]
            (text <|
                "satisfied: "
                    ++ (String.fromFloat <| Utility.roundTo 1 <| 100 * (Schelling.fractionSatisfied model.cells))
                    ++ "%"
            )
        , el [ Font.size 14, width (px numberDisplayWidth) ]
               (text <|
                   "threshold: "
                       ++ (String.fromFloat <| Utility.roundTo 1 <| 100 * model.schellingModel.threshold)
                       ++ "%"
               )
        , el [ Font.size 14, width (px numberDisplayWidth) ]
             (text <|
                 "similar: "
                     ++ (String.fromFloat <| Utility.roundTo 1 <| 100 *
                        (Schelling.aggregateFractionLikeMe model.schellingModel model.cells))
                     ++ "%"
             )

        ]

numberDisplayWidth = 95

labelWidth =
    (px 80)


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


inputTreshold : Model -> Element Msg
inputTreshold model =
    Input.text [ Font.size 12, height (px 24), width (px 50) ]
        { onChange = InputThreshold
        , text = model.numberLikeMeString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ moveDown 8 ] (text "Like me (1-8): ")
        }


inputFractionOccupied : Model -> Element Msg
inputFractionOccupied model =
    Input.text [ Font.size 12, height (px 24), width (px 50) ]
        { onChange = InputFractionUnoccupied
        , text = model.fractionUnoccupiedString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ moveDown 8 ] (text "Empty (%): ")
        }


goButton : Model -> Element Msg
goButton model =
    row [ centerX ]
        [ Input.button smallButtonStyle
            { onPress = Just ToggleAppState
            , label = el [ centerX, centerY ] (text (goButtonLabel model))
            }
        ]


resetButton : Element Msg
resetButton =
    row [ centerX ]
        [ Input.button smallButtonStyle
            { onPress = Just Reset
            , label = el [ centerX, centerY ] (text "Reset")
            }
        ]


goButtonLabel : Model -> String
goButtonLabel model =
    case model.appState of
        Go ->
            "Stop"

        Stop ->
            "Go"


randomInitialModelButton : Model -> Element Msg
randomInitialModelButton model =
    row [ centerX ]
        [ Input.button (activeButtonStyle (model.initialState == Random))
            { onPress = Just (SetInitialState Random)
            , label = el [ centerX, centerY ] (text "Random")
            }
        ]


stableInitialModelButton : Model -> Element Msg
stableInitialModelButton model =
    row [ centerX ]
        [ Input.button (activeButtonStyle (model.initialState == Stable))
            { onPress = Just (SetInitialState Stable)
            , label = el [ centerX, centerY ] (text "Unstable 0")
            }
        ]


unstableInitialModelButton1 : Model -> Element Msg
unstableInitialModelButton1 model =
    row [ centerX ]
        [ Input.button (activeButtonStyle (model.initialState == (Unstable 1)))
            { onPress = Just (SetInitialState (Unstable 1))
            , label = el [ centerX, centerY ] (text "Unstable 1")
            }
        ]


unstableInitialModelButton2 : Model -> Element Msg
unstableInitialModelButton2 model =
    row [ centerX ]
        [ Input.button (activeButtonStyle (model.initialState == Unstable 2))
            { onPress = Just (SetInitialState (Unstable 2))
            , label = el [ centerX, centerY ] (text "Unstable 2")
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ Background.color (rgb255 240 240 240)
    , paddingXY 20 20
    , spacing 10
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


smallButtonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 4
    , Font.size 12
    , height (px 20)
    ]


activeButtonStyle selected =
    let
        bgColor =
            if selected then
                Background.color (rgb255 140 0 0)
            else
                Background.color (rgb255 40 40 40)
    in
        [ bgColor
        , Font.color (rgb255 255 255 255)
        , paddingXY 15 4
        , Font.size 12
        , height (px 20)
        ]
