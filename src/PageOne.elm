module PageOne exposing (Model, Msg(..), init, update, view)

import BufferedInput
import Html                exposing (Html, div, span, input, text, strong)
import Html.Attributes     exposing (type', disabled, min, max, class, value, style)
import Html.Events         exposing (on, onClick, targetValue)
import Result
import String
import Maybe
import Json.Decode as Json exposing ((:=))
import Html.App as Html
import Task
import Http


-- MODEL


type alias Model =
  { pagename     : String
  , bufferedName : BufferedInput.Model
  , color        : Color
  , fetchNo      : Int
  , isFetching   : Bool
  }


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


init : String -> (Model, Cmd Msg)
init pagename =
  (Model pagename (BufferedInput.init "foo" "francis") Red 3 False, Cmd.none)


-- EFFECTS


getName : Int -> Cmd Msg
getName no =
  userUrl no |> Http.get decodeName |> Task.perform FailUser NewUser


-- MSG


type Msg = SetColor Color
         | SetFetchNo Int
         | BufferedInput BufferedInput.Msg
         | NewUser String
         | FailUser Http.Error
         | SetCaption String
         | RequestUser


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let bufferedName = model.bufferedName
      firstName name = name
        |> String.split " "
        |> List.head
        |> Maybe.withDefault ""
      updateBufferedName act = BufferedInput.update act bufferedName
  in
    case msg of
      SetColor color -> { model | color = color} ! []
      SetFetchNo fetchNo -> { model | fetchNo = fetchNo } ! []
      BufferedInput act ->
        { model | bufferedName = updateBufferedName act } ! []
      RequestUser -> { model | isFetching = True } ! [getName model.fetchNo]
      FailUser _ -> { model | isFetching = False } ! []
      NewUser name ->
        let bufferedName = { bufferedName | value = firstName name }
        in
          { model | isFetching = False, bufferedName = bufferedName } ! []
      SetCaption pagename -> { model | pagename = pagename } ! []


-- VIEW


captionInput : String -> Html Msg
captionInput value' =
  input [ type' "text"
        , on "input" (Json.map SetCaption targetValue)
        , value value'
        ] []


fetchRow : Int -> Bool -> Html Msg
fetchRow fetchNo isFetching =
  let inputMsg string = string |> String.toInt |> Result.withDefault 0 |> SetFetchNo
  in
    div [] [ span [] [ text "Id: " ]
                     , input [ type' "number"
                             , on "input" (Json.map inputMsg targetValue)
                             , Html.Attributes.min "1"
                             , Html.Attributes.max "11"
                             , value (toString fetchNo)
                             , fetchNo |> toString |> value
                             ] []
                     , span [] [ text " " ]
                     , input [ type' "button"
                             , value "Fetch"
                             , disabled isFetching
                             , onClick RequestUser
                             ] []
                     ]


colorTiles : Html Msg
colorTiles =
  let colorTile color = div [ class "tile"
                            , style [ ("background-color", colorCode color) ]
                            , onClick (SetColor color)
                            ] []
      tiles = List.map colorTile [ Green, Blue, Purple, Red ]
  in
    div [ class "tile-wrapper" ] tiles


name : Color -> String -> Html Msg
name color text' = div [ class "text" ]
     [ text "Hello from "
     , strong [ style [ ("color", colorCode color) ] ] [ text text' ]
     ]


view : Model -> Html Msg
view model =
  let nameInput = Html.map BufferedInput (BufferedInput.view model.bufferedName)
  in
    div [ class "page" ]
      [ captionInput model.pagename
      , name model.color model.bufferedName.value
      , colorTiles
      , nameInput
      , fetchRow model.fetchNo model.isFetching
      ]
