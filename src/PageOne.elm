module PageOne (Model, Action, init, update, view, Context) where

import Signal          exposing (..)
import Html            exposing (Html, div, input, text, strong)
import Html.Attributes exposing (type', class, value, style)
import Html.Events     exposing (on, onClick, targetValue)


-- MODEL

type Color = Green
           | Blue
           | Purple
           | Red

type alias Model =
  { pagename : String
  , name     : String
  , color    : Color
  }


init : String -> Model
init pagename =
  Model pagename "francis" Red


-- ACTION


type Action = SetColor Color


type alias Context =
  { actions    : Signal.Address Action
  , setCaption : Signal.Address String
  }


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    SetColor color -> { model | color = color }


-- VIEW


view : Context -> Model -> Html
view context model =
  let inputHandler = Signal.message context.setCaption
      caption = input [ type' "text"
                      , on "input" targetValue inputHandler
                      , value model.pagename
                      ] []
      colorCode color = case color of
        Green -> "#859900"
        Blue -> "#268bd2"
        Purple -> "#6c71c4"
        Red -> "#dc322f"
      name = div [ class "text" ]
        [ text "Hello from "
        , strong [ style [ ("color", colorCode model.color) ] ] [ text model.name ]
        ]
      colorTile color = div [ class "tile"
                            , style [ ("background-color", colorCode color) ]
                            , onClick context.actions (SetColor color)
                            ] []
      tiles = List.map colorTile [ Green, Blue, Purple, Red ]
      colorTiles = div [ class "tile-wrapper" ] tiles
  in
    div [ class "page" ]
      [ caption
      , name
      , colorTiles
      ]

