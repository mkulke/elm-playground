module Main exposing (..)

import Header
import PageOne
import PageTwo
import Html        exposing (Html, div, span, button, text)
import Html.App as Html


-- MAIN


main = Html.program
  { init  = init
  , update = update
  , view   = view
  , subscriptions = \_ -> Sub.none
  }


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


-- MSG


type Msg = Header Header.Msg
         | PageOne PageOne.Msg


-- VIEW


view : Model -> Html Msg
view model =
  div []
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
      PageOne msg' ->
        -- We handle PageOne msg in a special way, since msg's from that module will effect
        -- the Header model as well.
        let (updatedModel, cmd') = PageOne.update msg' model.pageOne
        in
          case msg' of
            PageOne.SetCaption caption -> ({ model
                                           | pageOne = updatedModel
                                           , header = { header | pages = newPages caption }
                                           }, Cmd.none)
            _ -> ({ model | pageOne = updatedModel }, Cmd.map PageOne cmd')
