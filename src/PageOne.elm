module PageOne (Model, Action, init, update, view, Context) where

import BufferedInput
import Signal          exposing (..)
import Html            exposing (Html, div, input, text, strong)
import Html.Attributes exposing (type', class, value, style)
import Html.Events     exposing (on, onClick, targetValue)
import Maybe


-- MODEL


type Color = Green
           | Blue
           | Purple
           | Red


type alias Model =
  { pagename     : String
  , bufferedName : BufferedInput.Model
  , color        : Color
  }


init : String -> Model
init pagename =
  Model pagename (BufferedInput.init "foo" "francis") Red


-- ACTION


type Action = SetColor Color
            | BufferedInput BufferedInput.Action


type alias Context =
  { actions    : Signal.Address Action
  , setCaption : Signal.Address String
  }


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    SetColor color -> { model | color = color }
    BufferedInput act -> { model | bufferedName = BufferedInput.update act model.bufferedName }


-- VIEW


view : Context -> Model -> Html
view context model =
  let captionInputHandler = Signal.message context.setCaption
      captionInput = input [ type' "text"
                           , on "input" targetValue captionInputHandler
                           , value model.pagename
                           ] []
      colorCode color = case color of
        Green -> "#859900"
        Blue -> "#268bd2"
        Purple -> "#6c71c4"
        Red -> "#dc322f"
      name = div [ class "text" ]
        [ text "Hello from "
        , strong [ style [ ("color", colorCode model.color) ] ] [ text model.bufferedName.value ]
        ]
      colorTile color = div [ class "tile"
                            , style [ ("background-color", colorCode color) ]
                            , onClick context.actions (SetColor color)
                            ] []
      tiles = List.map colorTile [ Green, Blue, Purple, Red ]
      colorTiles = div [ class "tile-wrapper" ] tiles
      nameInput = BufferedInput.view (Signal.forwardTo context.actions BufferedInput) model.bufferedName
  in
    div [ class "page" ]
      [ captionInput
      , name
      , colorTiles
      , nameInput
      ]
