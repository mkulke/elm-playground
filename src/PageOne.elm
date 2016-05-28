module PageOne exposing (Model, Msg(..), init, update, view)

import BufferedInput
import Html                exposing (Html, div, span, input, text, strong)
import Html.Attributes     exposing (type', disabled, min, max, class, value, style)
import Html.Events         exposing (on, onClick, targetValue)
import Result
import String
import Maybe
import Json.Decode as Json exposing ((:=))
-- import Effects             exposing (Effects)
-- import Task
-- import Http


-- MODEL


type alias Model =
  { pagename     : String
  , bufferedName : BufferedInput.Model
  , color        : Color
  , fetchNo      : Int
  , isFetching   : Bool
  }


type Color = Green
           | Blue
           | Purple
           | Red


decodeName : Json.Decoder String
decodeName =
  "name" := Json.string


userUrl : Int -> String
userUrl fetchNo =
  "http://jsonplaceholder.typicode.com/users/" ++ toString fetchNo


toValidFetchNo : Int -> Result String Int
toValidFetchNo no =
  if no >= 1 && no <= 11 then Ok no else Err "no must be between 1 and 11"


toFetchNo : String -> Result String Int
toFetchNo string =
  String.toInt string `Result.andThen` toValidFetchNo


colorCode : Color -> String
colorCode color = case color of
  Green  -> "#859900"
  Blue   -> "#268bd2"
  Purple -> "#6c71c4"
  Red    -> "#dc322f"


init : String -> (Model, Cmd Msg)
init pagename =
  (Model pagename (BufferedInput.init "foo" "francis") Red 3 False, Cmd.none)


-- EFFECTS


-- getName : Int -> Effects Action
-- getName no =
--   userUrl no
--     |> Http.get decodeName
--     |> Task.toMaybe
--     |> Task.map NewUser
--     |> Effects.task


-- MSG


type Msg = SetColor Color
         | SetFetchNo Int
         | BufferedInput BufferedInput.Msg
         | NewUser (Maybe String)
         | SetCaption String
         | RequestUser
         | Noop


-- type alias Context =
--   { actions    : Msg
--   , setCaption : String
--   }


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetColor color -> { model | color = color}
    _ -> model
-- update : Action -> Model -> (Model, Effects Action)
-- update action model =
--   let bufferedName = model.bufferedName
--       firstName maybeName = maybeName
--         |> Maybe.withDefault bufferedName.value
--         |> String.split " "
--         |> List.head
--         |> Maybe.withDefault ""
--       updateBufferedName act = BufferedInput.update act bufferedName
--   in
--     case action of
--       SetColor color -> ({ model | color = color }, Effects.none)
--       SetFetchNo fetchNo -> ({ model | fetchNo = fetchNo }, Effects.none)
--       BufferedInput act -> ({ model | bufferedName = updateBufferedName act }, Effects.none)
--       NewUser maybeName -> ({model | bufferedName = { bufferedName | value = firstName maybeName }
--                                    , isFetching = False
--                                    }, Effects.none)
--       RequestUser -> ({ model | isFetching = True }, getName model.fetchNo)
--       Noop -> (model, Effects.none)


-- VIEW


-- captionInput : Address String -> String -> Html
captionInput : String -> Html Msg
captionInput value' =
  -- let captionInputHandler = Signal.message address
  -- in
  input [ type' "text"
        -- , on "input" targetValue captionInputHandler
        , on "input" (Json.map SetCaption targetValue)
        , value value'
        ] []


-- fetchRow : Address Action -> Int -> Bool -> Html
-- fetchRow address fetchNo isFetching =
--   let idAction input = case toFetchNo input of
--                          Err e -> Noop
--                          Ok i -> SetFetchNo i
--       idInputHandler input = Signal.message address (idAction input)
--   in
--     div [] [ span [] [ text "Id: " ]
--                      , input [ type' "number"
--                              , on "input" targetValue idInputHandler
--                              , Html.Attributes.min "1"
--                              , Html.Attributes.max "11"
--                              , value (toString fetchNo)
--                              , fetchNo |> toString |> value
--                              ] []
--                      , span [] [ text " " ]
--                      , input [ type' "button"
--                              , value "Fetch"
--                              , disabled isFetching
--                              , onClick address RequestUser
--                              ] []
--                      ]


colorTiles : Html Msg
colorTiles =
  let colorTile color = div [ class "tile"
                            , style [ ("background-color", colorCode color) ]
                            , onClick (SetColor color)
                            ] []
      tiles = List.map colorTile [ Green, Blue, Purple, Red ]
  in
    div [ class "tile-wrapper" ] tiles


name : Color -> String -> Html Msg
name color text' = div [ class "text" ]
     [ text "Hello from "
     , strong [ style [ ("color", colorCode color) ] ] [ text text' ]
     ]


-- view : Context -> Model -> Html
view : Model -> Html Msg
view model =
  -- let nameInput = BufferedInput.view (Signal.forwardTo context.actions BufferedInput) model.bufferedName
  -- let nameInput = map BufferedInput.view (Signal.forwardTo context.actions BufferedInput) model.bufferedName
  -- in
  div [ class "page" ]
    [ captionInput model.pagename
    , name model.color model.bufferedName.value
    , colorTiles
    -- , colorTiles context.actions
    -- , nameInput
    -- , fetchRow context.actions model.fetchNo model.isFetching
    ]
