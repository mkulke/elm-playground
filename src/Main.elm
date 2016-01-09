module Main where

import Header
import Html            exposing (Html, div, span, button, text)
import Html.Events     exposing (onClick)
import Signal          exposing (Address)
import StartApp.Simple exposing (start)


-- MAIN


main = start
  { model  = init
  , update = update
  , view   = view
  }


-- MODEL


type alias Model =
  { header : Header.Model
  , count  : Int
  }


init : Model
init =
  Model (Header.Model [{ caption = "Page 1" }
                      ,{ caption = "Page Deux" }] 1) 10


-- ACTION


type Action = Increment
            | Decrement
            | Header Header.Action


-- VIEW


view : Address Action -> Model -> Html
view address model =
  div []
    [ Header.view (Signal.forwardTo address Header) model.header
    , button [ onClick address Decrement ] [ text "-" ]
    , span [] [ text (toString model.count) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    Increment -> { model | count = model.count + 1 }
    Decrement -> { model | count = model.count - 1 }
    Header headerAction -> { model | header = Header.update headerAction model.header }
