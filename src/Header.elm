module Header (Model, Action, update, view) where

import Signal          exposing (..)
import Html            exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events     exposing (onClick)


-- MODEL


type alias Page = { caption : String }


type alias Model =
  { pages  : List Page
  , active : Int
  }


init : List Page -> Int -> Model
init pages index =
  Model pages index


-- ACTION


type Action = SetActive Int


-- UPDATE


update : Action -> Model -> Model
update action model =
  let
    sanitize = \index ->
      if List.length model.pages > index && index >= 0 then index else 0
  in
    case action of
      SetActive index -> { model | active = (sanitize index) }


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let
    active = \i -> if i == model.active then " active" else ""
    item = \i p -> div [ onClick address (SetActive i)
                       , class ("item" ++ active i)
                       ] [ text p.caption ]
    items = List.indexedMap item model.pages
  in
    div [ class "header" ] items
