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
import Schelling exposing( Cell)
import Array exposing(Array)
import Time exposing(Posix)
import Random
import Utility
import Text

tickInterval = 333


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
    , threshold : Float
    , numberLikeMeString : String
    , fractionUnoccupiedString : String
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
  let
    dm = Schelling.defaultModel
  in
    ( { input = "App started"
      , output = "App started"
      , cells = Schelling.initialize Schelling.defaultModel (Utility.orbit Utility.ff (2*dm.nRows*dm.nCols) 23)
      , schellingModel = Schelling.defaultModel
      , threshold = 0.4
      , numberLikeMeString = "3"
      , fractionUnoccupied = 0.1
      , fractionUnoccupiedString = "10"
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
    | InputFractionUnoccupied String
    | UpdateModel
    | Tick Posix
    | NewRandomNumbers (List Int)
    | ToggleAppState
    | Reset



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
            ( { model | numberLikeMeString = numberLikeMeString, threshold = (stringToFloat numberLikeMeString 0)/8.0 }, Cmd.none )

        InputFractionUnoccupied str ->
                    ( { model | fractionUnoccupiedString = str
                         , fractionUnoccupied = (stringToFloat str 10)/100.0 }, Cmd.none )

        UpdateModel ->
            (  model , Cmd.none )

        Tick posix ->
            case model.appState of
                Stop -> (model, Cmd.none)
                Go ->
                 ({model | tickCount = model.tickCount + 1}, Random.generate NewRandomNumbers (Random.list 1000 (Random.int 0 100000)))

        NewRandomNumbers randList ->
            let
                sm = model.schellingModel
                n = sm.nRows*sm.nCols - 1
            in
              if model.appState == Stop then
                 (model, Cmd.none)
              else
                ({ model |
                   cells = Schelling.updateCells model.schellingModel
                             (Schelling.inputSequence model.schellingModel randList)
                             model.cells
                   }, Cmd.none)

        ToggleAppState ->
            case model.appState of
                Stop -> ( { model | appState = Go}, Cmd.none )
                Go -> ( { model | appState = Stop}, Cmd.none )

        Reset ->
            let
                sm = model.schellingModel
                smdm = model.schellingModel
            in
            ( {model | cells = Schelling.initialize sm (Utility.orbit Utility.ff (2*sm.nRows*sm.nCols) 23)
                          , tickCount = 0, appState = Stop}, Cmd.none)



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    row [ spacing 24, centerX, centerY ] [appPanel model, Text.panel]


appPanel : Model -> Element Msg
appPanel model =
    column mainColumnStyle [
                 title "Schelling model"
                , display model
                , controls model
                , indicators model
             ]



display : Model-> Element Msg
display model =
  column [moveRight 40, moveDown 20] [Schelling.renderAsHtml model.schellingModel model.cells |> Element.html]

controls : Model -> Element Msg
controls model =
    row [spacing 12, moveUp 12] [ goButton  model, resetButton, inputTreshold model, inputFractionOccupied model]

indicators : Model -> Element Msg
indicators model =
    row [spacing 18] [
                    el [Font.size 14] (text <| "cycle: " ++ String.fromInt model.tickCount)
                   , el [Font.size 14]
                       (text <| "satisfied: "
                          ++ (String.fromFloat <| Utility.roundTo 1 <| 100*(Schelling.fractionSatisfied model.cells))++"%")
                  ]

labelWidth = (px 80)

title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


inputTreshold : Model -> Element Msg
inputTreshold model =
    Input.text [Font.size 12, height (px 24), width (px 50)]
        { onChange = InputThreshold
        , text = model.numberLikeMeString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [moveDown 8] (text "Like me (1-8): ")
        }

inputFractionOccupied : Model -> Element Msg
inputFractionOccupied model =
    Input.text [Font.size 12, height (px 24), width (px 50)]
        { onChange = InputFractionUnoccupied
        , text = model.fractionUnoccupiedString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [moveDown 8] (text "Empty (%): ")
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
    [  Background.color (rgb255 240 240 240)
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
