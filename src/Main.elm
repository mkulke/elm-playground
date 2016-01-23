module Main where

import Html                exposing (Html, text, p)
import Signal              exposing (Address)
import Effects             exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import StartApp            exposing (start)
import Task
import Http


-- MAIN


app = start
  { init  = init
  , update = update
  , view   = view
  , inputs = []
  }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- MODEL


type alias Model = String


init : (Model, Effects Action)
init =
  ("...", getName)


decodeName : Json.Decoder String
decodeName =
  "name" := Json.string


userUrl : String
userUrl = "http://jsonplaceholder.typicode.com/users/1"


-- ACTION


type Action = NewUser (Maybe String)


-- EFFECTS


getName : Effects Action
getName =
  Http.get decodeName userUrl
    |> Task.toMaybe
    |> Task.map NewUser
    |> Effects.task


-- VIEW


view : Address Action -> Model -> Html
view address model =
  p [] [ text model ]


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  let name maybeName = Maybe.withDefault "error" maybeName
  in
    case action of
      NewUser maybeName -> (name maybeName, Effects.none)
