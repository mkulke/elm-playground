module PageOne (Model, Action, update, view) where

import Signal          exposing (..)
import Html            exposing (Html, div, input, text)
import Html.Attributes exposing (type', class, value)
import Html.Events     exposing (onClick, onKeyPress)


-- MODEL


type alias Model =
  { pagename : String }


init : String -> Model
init pagename =
  Model pagename

-- ACTION


type Action = ChangeCaption String


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    ChangeCaption caption -> { model | pagename = caption }


-- VIEW


view : Address Action -> Model -> Html
view address model =
  div [ class "page" ] [ input [ type' "text"
                               , onKeyPress address (\k -> ChangeCaption "testo")
                               , value model.pagename
                               ] [] ]
