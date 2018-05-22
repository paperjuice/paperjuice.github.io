import Html exposing (div, text, Html)
import Navigation exposing (Location)
import UrlParser exposing (..)


main =
  Navigation.program Route 
  { view = view
  , update = update
  , subscriptions = (\_-> Sub.none)
  , init = init
  }



matchers : Parser (Route -> a) a
matchers =
  oneOf
    [ map PlayersRoute top
    , map PlayerRoute (s "players" </> string)
    , map PlayersRoute (s "players")
    ]

type alias PlayerId 
  = String

type Route
  = NotFoundRoute
  | PlayersRoute
  | PlayerRoute PlayerId
parseLocation : Location -> Route
parseLocation  location =
  case (parseHash matchers location) of
    Just route ->
      route

    Nothing -> NotFoundRoute



-- Msg --
type Msg
  = Route Navigation.Location 

-- Init --
type alias Model =
  { route : Route
  }

init location =
  update (Route location) (Model PlayersRoute) 

-- View --
view : Model -> Html msg
view model =
  case model.route of
    PlayersRoute -> div [] [ text "hello" ]

    PlayerRoute string -> div [] [ text ("Page 1 " ++ string) ]

    NotFoundRoute -> div [] [ text "Not found :( " ]


-- Update --
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Route location ->
      let
          newRoute = parseLocation location
      in
          ( { model | route = newRoute }, Cmd.none )

