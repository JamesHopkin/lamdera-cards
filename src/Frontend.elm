module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Lamdera
import Url

import BasicTypes
import Display
import Types


-- import games here too, or somehow wrap them in Shared?
import Bezique


type alias Model =
    Types.FrontendModel


-- https://cards.lamdera.app

app =
    Lamdera.frontend
        { init = init
        , onUrlChange = \_ -> Types.Noop
        , onUrlRequest = \_ -> Types.Noop
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }

initGame : String -> Types.GameFrontendModel
initGame urlStr =
    -- assume bezique for now
    Types.BeziqueFrontendModel Bezique.frontendInit

init : Url.Url -> Nav.Key -> ( Model, Cmd Types.FrontendMsg )
init url _ =
    ( { game = initGame (Url.toString url)
      , debugInfo =
        { clientId = "unset!"
        , numGamesCreatedAtConnectionTime = 0
        }
      }
    , Cmd.none
    )


update : Types.FrontendMsg -> Model -> ( Model, Cmd Types.FrontendMsg )
update msg model = 
    case msg of
        Types.GameFrontend gameMessage ->
            Types.frontendUpdate gameMessage model 

        Types.Noop ->
            ( model, Cmd.none )

        Types.ForceInit ->
            ( model, Lamdera.sendToBackend Types.BackendForceInit )


updateFromBackend : Types.ToFrontend -> Model -> ( Model, Cmd Types.FrontendMsg )
updateFromBackend msg model = 
    case msg of
        Types.GameToFrontend gameMsg ->
            Types.updateFromBackend gameMsg model

        Types.InitDebugInfo info ->
            ( { model
              | debugInfo = info
              }
            , Cmd.none
            )


view : Model -> Browser.Document Types.FrontendMsg
view model =
    { title = "woo"
    , body =
        [   Html.div [] [Html.button [onClick Types.ForceInit] [Html.text "force init"]]
        ,   case model.game of
                Types.BeziqueFrontendModel bezique ->
                    Bezique.view (Types.BeziqueFrontend >> Types.GameFrontend) bezique

                _ ->
                    Html.div [] [Html.text "other stuff goes here!"]
        ]
    }