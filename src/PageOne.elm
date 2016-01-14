module PageOne (Model, Action, update, view, Context) where

import Signal          exposing (..)
import Html            exposing (Html, div, input, text)
import Html.Attributes exposing (type', class, value)
import Html.Events     exposing (on, targetValue)


-- MODEL


type alias Model =
  { pagename : String }


init : String -> Model
init pagename =
  Model pagename


-- ACTION


type Action = ChangeCaption String


type alias Context =
  { actions    : Signal.Address Action
  , setCaption : Signal.Address String
  }


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    ChangeCaption caption -> { model | pagename = caption }


-- VIEW


view : Context -> Model -> Html
view context model =
  let inputHandler = Signal.message context.setCaption
      caption = input [ type' "text"
                      , on "input" targetValue inputHandler
                      , value model.pagename
                      ] []
  in
    div [ class "page" ] [ caption ]

