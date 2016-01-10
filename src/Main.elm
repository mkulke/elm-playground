module Main where

import Header
import PageOne
import PageTwo
import Html        exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Signal      exposing (Address)
import Effects     exposing (Effects, Never)
import StartApp    exposing (start)


-- MAIN


app = start
  { init  = init
  , update = update
  , view   = view
  , inputs = []
  }

main =
  app.html


-- MODEL


type alias Model =
  { header : Header.Model
  , count  : Int
  }


init : (Model, Effects Action)
init =
  ( Model (Header.Model [{ caption = "Page 1" }
                      ,{ caption = "Page Deux" }] 1) 10
  , Effects.none
  )


-- ACTION


type Action = Increment
            | Decrement
            | Header Header.Action


-- VIEW


view : Address Action -> Model -> Html
view address model =
  div []
    [ Header.view (Signal.forwardTo address Header) model.header
    , case model.header.active of
      1 -> PageTwo.view
      _ -> PageOne.view
    , button [ onClick address Decrement ] [ text "-" ]
    , span [] [ text (toString model.count) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Increment -> ({ model | count = model.count + 1 }, Effects.none)
    Decrement -> ({ model | count = model.count - 1 }, Effects.none)
    Header headerAction -> ({ model | header = Header.update headerAction model.header }, Effects.none)
