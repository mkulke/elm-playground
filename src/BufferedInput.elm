module BufferedInput (Model, Action, init, update, view) where

import Signal          exposing (..)
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


-- ACTION


type Action = SetBuffer String
            | SetValue String
            | Noop



-- UPDATE


update : Action -> Model -> Model
update action model =
  let capitalize = \t -> case uncons t of
        Nothing -> ""
        Just (h, t) -> cons (toUpper h) t
  in
    case action of
      SetBuffer buffer -> { model | buffer = buffer }
      SetValue value -> { model | value = capitalize value }
      Noop -> model


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let bufferHandler = Signal.message address << SetBuffer
      isEnter code = if code == 13 then SetValue model.buffer else Noop
      confirmHandler = Signal.message address << isEnter
  in input [ type' "text"
           , on "input" targetValue bufferHandler
           , on "keyup" keyCode confirmHandler
           , value model.buffer
           ] []
