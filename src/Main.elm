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
  { header  : Header.Model
  , pageOne : PageOne.Model
  , count   : Int
  }


init : (Model, Effects Action)
init =
  let
    pageOne = PageOne.Model "Page 1"
    pages =
      [{ caption = pageOne.pagename }
      ,{ caption = "Page 2" }
      ]
    header = Header.Model pages 0
  in
    ( Model header pageOne 10
    , Effects.none
    )


-- ACTION


type Action = Increment
            | Decrement
            | Header Header.Action
            | SetPageCaption String
            | PageOne PageOne.Action


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let context =
        PageOne.Context
          (Signal.forwardTo address PageOne)
          (Signal.forwardTo address SetPageCaption)
  in
    div []
      [ Header.view (Signal.forwardTo address Header) model.header
      , case model.header.active of
        1 -> PageTwo.view
        _ -> PageOne.view context model.pageOne
      , button [ onClick address Decrement ] [ text "-" ]
      , span [] [ text (toString model.count) ]
      , button [ onClick address Increment ] [ text "+" ]
      ]


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  let newHeader caption = Header.Model [ { caption = caption }
                                       , { caption = "Page 2" }
                                       ] model.header.active
  in
    case action of
      Increment -> ({ model | count = model.count + 1 }, Effects.none)
      Decrement -> ({ model | count = model.count - 1 }, Effects.none)
      Header act -> ({ model | header = Header.update act model.header }, Effects.none)
      PageOne act -> ({ model | pageOne = PageOne.update act model.pageOne }, Effects.none)
      SetPageCaption caption -> ({ model
                                 | header = newHeader caption
                                 , pageOne = PageOne.Model caption
                                 }, Effects.none)
