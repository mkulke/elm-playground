module PageOne (Model, Action, init, update, view, Context) where

import BufferedInput
import Signal              exposing (..)
import Html                exposing (Html, div, span, input, text, strong)
import Html.Attributes     exposing (type', disabled, min, max, class, value, style)
import Html.Events         exposing (on, onClick, targetValue)
import Result
import String
import Maybe
import Json.Decode as Json exposing ((:=))
import Effects             exposing (Effects)
import Task
import Http


-- MODEL


type Color = Green
           | Blue
           | Purple
           | Red


decodeName : Json.Decoder String
decodeName =
  "name" := Json.string


userUrl : Int -> String
userUrl fetchNo =
  "http://jsonplaceholder.typicode.com/users/" ++ toString fetchNo


toValidFetchNo : Int -> Result String Int
toValidFetchNo no =
  if no >= 1 && no <= 11 then Ok no else Err "no must be between 1 and 11"


toFetchNo : String -> Result String Int
toFetchNo string =
  String.toInt string `Result.andThen` toValidFetchNo


colorCode : Color -> String
colorCode color = case color of
  Green  -> "#859900"
  Blue   -> "#268bd2"
  Purple -> "#6c71c4"
  Red    -> "#dc322f"


type alias Model =
  { pagename     : String
  , bufferedName : BufferedInput.Model
  , color        : Color
  , fetchNo      : Int
  , isFetching   : Bool
  }


init : String -> (Model, Effects Action)
init pagename =
  (Model pagename (BufferedInput.init "foo" "francis") Red 3 False, Effects.none)


-- EFFECTS


getName : Int -> Effects Action
getName no =
  Http.get decodeName (userUrl no)
    |> Task.toMaybe
    |> Task.map NewUser
    |> Effects.task


-- ACTION


type Action = SetColor Color
            | SetFetchNo Int
            | BufferedInput BufferedInput.Action
            | NewUser (Maybe String)
            | RequestUser
            | Noop


type alias Context =
  { actions    : Signal.Address Action
  , setCaption : Signal.Address String
  }


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  let bufferedName = model.bufferedName
      name maybeName = { bufferedName | value = Maybe.withDefault bufferedName.value maybeName }
      updateBufferedName act = BufferedInput.update act bufferedName
  in
    case action of
      SetColor color -> ({ model | color = color }, Effects.none)
      SetFetchNo fetchNo -> ({ model | fetchNo = fetchNo }, Effects.none)
      BufferedInput act -> ({ model | bufferedName = updateBufferedName act }, Effects.none)
      NewUser maybeName -> ({model | bufferedName = name maybeName
                                   , isFetching = False
                                   }, Effects.none)
      RequestUser -> ({ model | isFetching = True }, getName model.fetchNo)
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
                               , value (toString model.fetchNo)
                               , model.fetchNo |> toString |> value
                               ] []
                       , span [] [ text " " ]
                       , input [ type' "button"
                               , value "Fetch"
                               , disabled model.isFetching
                               , onClick context.actions RequestUser
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
