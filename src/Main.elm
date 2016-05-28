module Main exposing (..)

import Header
import PageOne
import PageTwo
import Html        exposing (Html, div, span, button, text)
-- import Html.Events exposing (onClick)
-- import Effects     exposing (Effects, Never)
-- import StartApp    exposing (start)
-- import Task
import Html.App as Html


-- MAIN


main = Html.program
  { init  = init
  , update = update
  , view   = view
  , subscriptions = \_ -> Sub.none
  }


-- port tasks : Signal (Task.Task Never ())
-- port tasks =
--   app.tasks


-- MODEL


type alias Model =
  { header  : Header.Model
  , pageOne : PageOne.Model
  }


init : (Model, Cmd Msg)
init =
  let
    pageOne = fst (PageOne.init "Page 1")
    pages =
      [{ caption = pageOne.pagename }
      ,{ caption = "Page 2" }
      ]
    header = Header.Model pages 0
  in
    ( Model header pageOne
    , Cmd.none
    )


-- ACTION


type Msg = Header Header.Msg
         -- | SetPageCaption String PageOne.Msg
         -- | PageOne PageOne.Action
         | PageOne PageOne.Msg


-- VIEW


view : Model -> Html Msg
view model =
  -- let context =
  --       PageOne.Context
  --         (Signal.forwardTo address PageOne)
  --         (Signal.forwardTo address SetPageCaption)
  -- in
  div []
    -- [ Header.view (Signal.forwardTo address Header) model.header
    [ Html.map Header (Header.view model.header)
    , case model.header.active of
      1 -> PageTwo.view
      _ -> Html.map PageOne (PageOne.view model.pageOne)
    ]


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let pageOne = model.pageOne
      header = model.header
      setCaptionOnFirst caption i page = if i == 0 then { page | caption = caption } else page
      newPages caption = List.indexedMap (setCaptionOnFirst caption) header.pages
  in
    case msg of
      Header msg' -> ({ model | header = Header.update msg' model.header }, Cmd.none)
      PageOne msg' -> case msg' of
        PageOne.SetCaption caption -> ({ model
                                       | header = { header | pages = newPages caption }
                                       , pageOne = { pageOne | pagename = caption }
                                       }, Cmd.none)
        _ -> ({model | pageOne = PageOne.update msg' model.pageOne}, Cmd.none)
      -- PageOne act ->
      --   let (pageOne, effect) = PageOne.update act model.pageOne
      --   in
      --     ({ model | pageOne = pageOne }, Effects.map PageOne effect)
      -- SetPageCaption caption msg' -> ({ model
      --                                | header = { header | pages = newPages caption }
      --                                , pageOne = { pageOne | pagename = caption }
      --                                }, Cmd.none)
