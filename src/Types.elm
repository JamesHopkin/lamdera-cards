module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Random
import Url exposing (Url)

import Cards
import Game
import BasicTypes

{--
Shared fe/be file needs to include game files
Maybe call it Shared, so basic types like PlayerId can go in Types


--}
-- games
import Bezique
import TwoOhFourEight


type alias BackendModel =
    {   state : Maybe
            {   games:
                {   bezique : Bezique.BackendModel
                ,   twoOhFourEight : TwoOhFourEight.BackendModel
                }
            }
    ,   other : String
    }

--getGame : GameId -> BackendModel -> Maybe Game
--getGame id model = Array.get id model.games

--updateGame : GameId -> Game -> BackendModel -> Array Game
--updateGame id updated model = Array.set id updated model.games

-- kind of also what page, hence admin in list
type GameFrontendModel
    = BeziqueFrontendModel Bezique.FrontendModel
    | TwoOhFourEightFrontendModel
    | AdminFrontendModel Game.DebugInfo


type alias FrontendModel =
    { game : GameFrontendModel
    , debugInfo : Game.DebugInfo
    }

type BackendMsg
    = GameBackend GameBackendMsg
    | AdminBackend
    | OnConnection BasicTypes.PlayerId
    | InitialSeed Random.Seed


wrapBeziqueBackendUpdate : msg -> BackendModel
    -> ((Bezique.BackendMsg -> BackendMsg) -> msg -> Bezique.BackendModel
                                -> ( Bezique.BackendModel, Cmd BackendMsg ))
    -> ( BackendModel, Cmd BackendMsg )
wrapBeziqueBackendUpdate message model f =
    case model.state of
        Just ({games} as state) ->
            let
                ( bezique, cmd ) = f (BeziqueBackend >> GameBackend) message games.bezique
            in
            (   {   state = Just
                        {   state
                        |   games = 
                            { games
                            | bezique = bezique
                            }
                        }
                ,   other = "let's go!"
                }
            ,   cmd
            )

        _ ->
            ( model, Cmd.none )


wrapBeziqueFrontendUpdate : a -> FrontendModel
    -> ((Bezique.FrontendMsg -> FrontendMsg) -> a -> Bezique.FrontendModel
                                -> ( Bezique.FrontendModel, Cmd FrontendMsg ))
    -> ( FrontendModel, Cmd FrontendMsg )
wrapBeziqueFrontendUpdate message model f =
    case model.game of
        BeziqueFrontendModel beziqueModel ->
            let
                ( bezique, cmd ) = f (BeziqueFrontend >> GameFrontend) message beziqueModel
                game = model.game
            in
            ( { model
              | game = BeziqueFrontendModel bezique
              }
            , cmd )

        _ ->
            let
                dummy = Debug.log "expected Bezique update!" model.game
            in
            ( model, Cmd.none )

backendUpdate : GameBackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
backendUpdate msg model =
    case msg of
        BeziqueBackend beziqueMessage ->
            wrapBeziqueBackendUpdate beziqueMessage model Bezique.backendUpdate

updateFromFrontend : GameToBackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend message model = 
    case message of
        BeziqueToBackend beziqueMessage ->
            wrapBeziqueBackendUpdate beziqueMessage model Bezique.updateFromFrontend

updateFromBackend : GameToFrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend message model = 
    case message of
        BeziqueToFrontend beziqueMessage ->
            wrapBeziqueFrontendUpdate beziqueMessage model Bezique.updateFromBackend

frontendUpdate : GameFrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
frontendUpdate msg model =
    case msg of
        BeziqueFrontend beziqueMessage ->
            wrapBeziqueFrontendUpdate beziqueMessage model Bezique.frontendUpdate

        TwoOhFourEightFrontend -> {-twoOhFourEightMessage -> -}
            ( model, Cmd.none )

type GameBackendMsg
    = BeziqueBackend Bezique.BackendMsg

type GameFrontendMsg
    = BeziqueFrontend Bezique.FrontendMsg
    | TwoOhFourEightFrontend

type FrontendMsg
    = GameFrontend GameFrontendMsg
    | Noop
    | ForceInit

type GameToBackendMsg
    = BeziqueToBackend Bezique.ToBackendMsg

type ToBackend
    = GameToBackend GameToBackendMsg
    | BackendForceInit

type GameToFrontendMsg
    = BeziqueToFrontend Bezique.ToFrontendMsg

type ToFrontend
    = GameToFrontend GameToFrontendMsg
    | InitDebugInfo Game.DebugInfo
