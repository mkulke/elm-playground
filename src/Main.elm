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
  }


init : (Model, Effects Action)
init =
  let
    pageOne = PageOne.init "Page 1"
    pages =
      [{ caption = pageOne.pagename }
      ,{ caption = "Page 2" }
      ]
    header = Header.Model pages 0
  in
    ( Model header pageOne
    , Effects.none
    )


-- ACTION


type Action = Header Header.Action
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
      ]


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  let pageOne = model.pageOne
      header = model.header
      setCaptionOnFirst caption i page = if i == 0 then { page | caption = caption } else page
      newPages caption = List.indexedMap (setCaptionOnFirst caption) header.pages
  in
    case action of
      Header act -> ({ model | header = Header.update act model.header }, Effects.none)
      PageOne act -> ({ model | pageOne = PageOne.update act model.pageOne }, Effects.none)
      SetPageCaption caption -> ({ model
                                 | header = { header | pages = newPages caption }
                                 , pageOne = { pageOne | pagename = caption }
                                 }, Effects.none)
