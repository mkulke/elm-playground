module PageTwo (view) where

import Signal          exposing (..)
import Html            exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events     exposing (onClick)


-- VIEW


view : Html
view =
  div [ class "page" ] [ h2 [] [ text "Page II" ] ]
