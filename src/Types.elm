module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
import Url exposing (Url)
import Cards

import Bezique
import TwoOhFourEight

type FrontendGameState = NotStarted | MyTurn | OtherTurn | Won | Lost

-- will split this into constant on connect and updateable 
type alias DebugInfo =
    {   clientId : String
    ,   numGamesCreatedAtConnectionTime : Int
    }

type alias FrontendModel =
    { gameState : FrontendGameState
    , deck : Cards.Deck
    , selected : Int
    , debugInfo : DebugInfo
    , isAdmin : Bool
    }



-- use client ids for ease of debugging, switch to sessions later
type alias PlayerId = ClientId

-- next, try giving each player some cards (array of decks, with deck[num players] and draw pile for now?)
type Game =
    WipGame (List PlayerId) (Array Cards.Deck) Int

type alias BackendModel =
    { waitingPlayers : List PlayerId
    , games : Array Game
    }

type alias GameId = Int

getGame : GameId -> BackendModel -> Maybe Game
getGame id model = Array.get id model.games

updateGame : GameId -> Game -> BackendModel -> Array Game
updateGame id updated model = Array.set id updated model.games

type BackendMsg
    = OnConnection PlayerId
    | GameDeck GameId Cards.Deck


type FrontendMsg
    = Noop
    | Click Int

type ToBackend
    = NoOpToBackend


type ToFrontend
    = OnGameStateChanged FrontendGameState Cards.Deck
    | InitDebugInfo DebugInfo
