module BufferedInput exposing (Model, Msg, init, update, view)

import Json.Decode as Json
import Html            exposing (Html, input)
import Html.Attributes exposing (type', value)
import Html.Events     exposing (on, targetValue, keyCode)
import String          exposing (cons, uncons)
import Char            exposing (toUpper)


-- MODEL


type alias Model =
  { buffer : String
  , value  : String
  }


init : String -> String -> Model
init buffer value =
  Model buffer value


-- MSG


type Msg = SetBuffer String
         | SetValue String
         | Noop



-- UPDATE


update : Msg -> Model -> Model
update msg model =
  let capitalize = \t -> case uncons t of
        Nothing -> ""
        Just (h, t) -> cons (toUpper h) t
  in
    case msg of
      SetBuffer buffer -> { model | buffer = buffer }
      SetValue value -> { model | value = capitalize value }
      Noop -> model


-- VIEW


view : Model -> Html Msg
view model =
  let confirmHandler code = if code == 13 then SetValue model.buffer else Noop
  in input [ type' "text"
           , on "input" (Json.map SetBuffer targetValue)
           , on "keyup" (Json.map confirmHandler keyCode)
           , value model.buffer
           ] []
