module PageOne (view) where

import Signal          exposing (..)
import Html            exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events     exposing (onClick)


-- VIEW


view : Html
view =
  div [ class "page" ] [ h1 [] [ text "Page One" ] ]
