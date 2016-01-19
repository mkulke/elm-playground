module PageOne (Model, Action, init, update, view, Context) where

import BufferedInput
import Signal          exposing (..)
import Html            exposing (Html, div, span, input, text, strong)
import Html.Attributes exposing (type', disabled, min, max, class, value, style)
import Html.Events     exposing (on, onClick, targetValue)
import Result
import String
import Maybe
import Effects         exposing (Effects)


-- MODEL


type Color = Green
           | Blue
           | Purple
           | Red


toValidFetchNo : Int -> Result String Int
toValidFetchNo no =
  if no >= 1 && no <= 11 then Ok no else Err "no must be between 1 and 11"


toFetchNo : String -> Result String Int
toFetchNo string =
  String.toInt string `Result.andThen` toValidFetchNo


colorCode : Color -> String
colorCode color = case color of
  Green -> "#859900"
  Blue -> "#268bd2"
  Purple -> "#6c71c4"
  Red -> "#dc322f"


type alias Model =
  { pagename     : String
  , bufferedName : BufferedInput.Model
  , color        : Color
  , fetchNo      : Int
  }


init : String -> (Model, Effects Action)
init pagename =
  (Model pagename (BufferedInput.init "foo" "francis") Red 3, Effects.none)


-- ACTION


type Action = SetColor Color
            | SetFetchNo Int
            | BufferedInput BufferedInput.Action
            | Noop


type alias Context =
  { actions    : Signal.Address Action
  , setCaption : Signal.Address String
  }


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetColor color -> ({ model | color = color }, Effects.none)
    SetFetchNo fetchNo -> ({ model | fetchNo = fetchNo }, Effects.none)
    BufferedInput act -> ({ model | bufferedName = BufferedInput.update act model.bufferedName }, Effects.none)
    Noop -> (model, Effects.none)


-- VIEW


view : Context -> Model -> Html
view context model =
  let captionInputHandler = Signal.message context.setCaption
      captionInput = input [ type' "text"
                           , on "input" targetValue captionInputHandler
                           , value model.pagename
                           ] []
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
      idAction input = case toFetchNo input of
                         Err e -> Noop
                         Ok i -> SetFetchNo i
      idInputHandler input = Signal.message context.actions (idAction input)
      idInput = div [] [ span [] [ text "Id: " ]
                       , input [ type' "number"
                               , on "input" targetValue idInputHandler
                               , Html.Attributes.min "1"
                               , Html.Attributes.max "11"
                               -- , value (toString model.fetchNo)
                               , model.fetchNo |> toString |> value
                               ] []
                       , span [] [ text " " ]
                       , input [ type' "button"
                               , value "Fetch"
                               , disabled True
                               ] []
                       ]
  in
    div [ class "page" ]
      [ captionInput
      , name
      , colorTiles
      , nameInput
      , idInput
      ]
