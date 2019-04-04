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
import Schelling exposing(nRows, nCols, Cell)
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
      , cells = Schelling.cells
      , tickCount = 0
      , randomNumber = 0
      , cellIndex = 0
      , appState = Stop
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | InputText String
    | UpdateModel
    | Tick Posix
    | NewRandomNumbers (List Int)
    | ToggleAppState



type alias Flags =
    {}



subscriptions model =
    Time.every 100 Tick



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str, output = str }, Cmd.none )

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
            , column [moveRight 60, moveDown 10] [Schelling.renderAsHtml model.cells |> Element.html]
            ]
            , row [spacing 12, moveUp 12] [ goButton  model]
            , row [spacing 12] [

                el [Font.size 14, width labelWidth] (text <| "cycle: " ++ String.fromInt model.tickCount)
               , el [Font.size 14, width labelWidth] (text <| "satisfied: " ++ (String.fromFloat <| Utility.roundTo 3 <| Schelling.fractionSatisfied model.cells))
               , el [Font.size 14, width labelWidth] (text <| "threshold: " ++ (String.fromFloat <| Utility.roundTo 3 <| Schelling.modelThreshold))
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
    Input.text []
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [] (text "")
        }


goButton : Model -> Element Msg
goButton model =
    row [ centerX ]
        [ Input.button smallButtonStyle
            { onPress = Just ToggleAppState
            , label = el [ centerX, centerY ] (text (goButtonLabel model))
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
