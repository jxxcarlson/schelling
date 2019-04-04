module SchellingApp exposing (main)

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
import Schelling exposing( Cell, nRows, nCols)
import Array exposing(Array)
import Time exposing(Posix)
import Random
import Utility




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
    , threshold : Float
    , thresholdString : String
    , fractionUnoccupied : Float
    , fractionA : Float
    , tickCount : Int
    , randomNumber : Float
    , cellIndex : Int
    , appState : AppState
    }


type AppState = Go | Stop

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , cells = Schelling.initialize 0.4 0.1 0.5 (Utility.orbit Utility.ff (2*nRows*nCols) 23)
      , threshold = 0.4
      , thresholdString = "0.4"
      , fractionUnoccupied = 0.1
      , fractionA = 0.5
      , tickCount = 0
      , randomNumber = 0
      , cellIndex = 0
      , appState = Stop
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | InputThreshold String
    | UpdateModel
    | Tick Posix
    | NewRandomNumbers (List Int)
    | ToggleAppState
    | Reset



type alias Flags =
    {}



subscriptions model =
    Time.every 1000 Tick



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputThreshold str ->
            ( { model | thresholdString = str, threshold = String.toFloat str |> Maybe.withDefault 0.4}, Cmd.none )

        UpdateModel ->
            (  model , Cmd.none )

        Tick posix ->
            case model.appState of
                Stop -> (model, Cmd.none)
                Go ->
                 ({model | tickCount = model.tickCount + 1}, Random.generate NewRandomNumbers (Random.list 1000 (Random.int 0 100000)))

        NewRandomNumbers randList ->
            let
                n = nRows*nCols - 1
            in
              if model.appState == Stop then
                 (model, Cmd.none)
              else
                ({ model |
                   cells = Schelling.updateCells (Schelling.inputSequence n randList) model.cells
                   }, Cmd.none)

        ToggleAppState ->
            case model.appState of
                Stop -> ( { model | appState = Go}, Cmd.none )
                Go -> ( { model | appState = Stop}, Cmd.none )

        Reset ->
            ( {model | cells = Schelling.initialize
                                 model.threshold
                                 model.fractionUnoccupied
                                 model.fractionA
                                 (Utility.orbit Utility.ff (2*nRows*nCols) 23)
              , tickCount = 0, appState = Stop}, Cmd.none)



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 20 ]
            [ title "Schelling model"
            , column [moveRight 40, moveDown 10] [Schelling.renderAsHtml model.cells |> Element.html]
            ]
            , row [spacing 12, moveUp 12] [ goButton  model, resetButton, inputText model]
            , row [spacing 12] [

                el [Font.size 14, width labelWidth] (text <| "cycle: " ++ String.fromInt model.tickCount)
               , el [Font.size 14, width labelWidth]
                   (text <| "satisfied: "
                      ++ (String.fromFloat <| Utility.roundTo 1 <| 100*(Schelling.fractionSatisfied model.cells))++"%")
              ]
         ]

labelWidth = (px 120)

title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


inputText : Model -> Element Msg
inputText model =
    Input.text [Font.size 12, height (px 24), width (px 60)]
        { onChange = InputThreshold
        , text = model.thresholdString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [moveDown 8] (text "Threshold")
        }


goButton : Model -> Element Msg
goButton model =
    row [ centerX ]
        [ Input.button smallButtonStyle
            { onPress = Just ToggleAppState
            , label = el [ centerX, centerY ] (text (goButtonLabel model))
            }
        ]


resetButton :  Element Msg
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
        Go -> "Stop"
        Stop -> "Go"

--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , paddingXY 20 20
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
