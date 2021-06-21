module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
import Url exposing (Url)
import Cards

type FrontendGameState = NotStarted | InProgress | Won | Lost

-- will split this into constant on connect and updateable 
type alias DebugInfo =
    {   clientId : String
    ,   numGamesCreatedAtConnectionTime : Int
    }

type alias FrontendModel =
    { gameState : FrontendGameState
    , deck : Cards.Deck
    , debugInfo : DebugInfo
    }

{--

random cards plumbing

what do I do with a random response?
    need to work out what bits I can keep in Cards and what has to bubble up

--}



-- use client ids for ease of debugging, switch to sessions later
type alias PlayerId = ClientId

-- next, try giving each player some cards (array of decks, with deck[num players] and draw pile for now?)
type Game =
    WipGame (List PlayerId) (Array Cards.Deck)

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

type ToBackend
    = NoOpToBackend


type ToFrontend
    = OnGameStateChanged FrontendGameState Cards.Deck
    | InitDebugInfo DebugInfo
