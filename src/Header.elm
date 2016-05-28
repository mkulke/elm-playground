module Header exposing (Model, Msg, update, view)

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


-- MSG


type Msg = SetActive Int


-- UPDATE


update : Msg -> Model -> Model
update msg model =
  let
    sanitize = \index ->
      if List.length model.pages > index && index >= 0 then index else 0
  in
    case msg of
      SetActive index -> { model | active = (sanitize index) }


-- VIEW


view : Model -> Html Msg
view model =
  let
    active = \i -> if i == model.active then " active" else ""
    item = \i p -> div [ onClick (SetActive i)
                       , class ("item" ++ active i)
                       ] [ text p.caption ]
    items = List.indexedMap item model.pages
  in
    div [ class "header" ] items
