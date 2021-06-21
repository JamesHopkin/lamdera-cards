module Evergreen.V1.Types exposing (..)

import Array
import Evergreen.V1.Cards
import Lamdera


type FrontendGameState
    = NotStarted
    | InProgress
    | Won
    | Lost


type alias DebugInfo = 
    { clientId : String
    , numGamesCreatedAtConnectionTime : Int
    }


type alias FrontendModel =
    { gameState : FrontendGameState
    , deck : Evergreen.V1.Cards.Deck
    , debugInfo : DebugInfo
    }


type alias PlayerId = Lamdera.ClientId


type Game
    = WipGame (List PlayerId) (Array.Array Evergreen.V1.Cards.Deck)


type alias BackendModel =
    { waitingPlayers : (List PlayerId)
    , games : (Array.Array Game)
    }


type FrontendMsg
    = Noop


type ToBackend
    = NoOpToBackend


type alias GameId = Int


type BackendMsg
    = OnConnection PlayerId
    | GameDeck GameId Evergreen.V1.Cards.Deck


type ToFrontend
    = OnGameStateChanged FrontendGameState Evergreen.V1.Cards.Deck
    | InitDebugInfo DebugInfo