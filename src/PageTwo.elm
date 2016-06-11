module PageTwo exposing (view)

import Html            exposing (Html, div, h2, text)
import Html.Attributes exposing (class)


-- VIEW

view : Html msg
view =
  div [ class "page" ] [ h2 [] [ text "Page II" ] ]
