import Html exposing (div, text, Html)
import Navigation exposing (Location)


main =
  Navigation.program Route 
  { view = view
  , update = update
  , subscriptions = (\_-> Sub.none)
  , init = init
  }


-- Msg --
type Msg
  = Route Navigation.Location 

-- Init --
type alias Model =
  { path : String
  }

init location =
  update (Route location) (Model "/")

-- View --
view : Model -> Html msg
view model =
  case model.path of
    "/" -> div [] [ text "hello" ]

    "/1" -> div [] [ text "Page 1" ]

    _   -> div [] [ text "Not found :( " ]


-- Update --
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Route location ->
      ( { model | path = location.pathname }, Cmd.none )

