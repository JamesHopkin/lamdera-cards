module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Lamdera
import Types exposing (..)
import Url

import Display


--import Canvas

type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ _ =
    ( { gameState = NotStarted
      , deck = []
      , debugInfo =
        { clientId = "unset!"
        , numGamesCreatedAtConnectionTime = 0
        }
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model = 
    case msg of

        Noop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model = 
    case msg of
        OnGameStateChanged state deck ->
            ( { model | gameState = state, deck = deck }, Cmd.none )

        InitDebugInfo info ->
            ( { model | debugInfo = info }, Cmd.none )

view : Model -> Browser.Document FrontendMsg
view model =
    let
      stateString = case model.gameState of
        NotStarted -> "NotStarted"
        InProgress -> "InProgress"
        Won -> "Won"
        Lost -> "Lost"
    in
    { title = ""
    , body =
        [ Display.drawCards model.deck
        , Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            (
              [ Html.text (stateString ++ ", ")
              , Html.div [] [Html.text <| model.debugInfo.clientId ++ ", " ++
                (String.fromInt <| List.length model.deck)  ++ ", " ++
                (String.fromInt model.debugInfo.numGamesCreatedAtConnectionTime)
                ]
              , Html.div [] [Html.text
                  <| String.join " : "
                  <| List.map (\(s, n) -> String.fromInt s ++ ", " ++ String.fromInt n) model.deck
                ]
              ]
            )
        ]
    }
