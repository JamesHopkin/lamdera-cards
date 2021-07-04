module Game exposing (..)

import Dict exposing ( Dict )
import Lamdera
import Random

import BasicTypes

--gameBroadcast : Game -> ToFrontend -> List (Cmd BackendMsg)
--gameBroadcast (WipGame players _ _) payload =
--    List.map (\p -> Lamdera.sendToFrontend p payload) players

type FrontendGameState = NotStarted | MyTurn | OtherTurn | Won | Lost

type alias GameId = Int

-- will split this into constant on connect and updateable 
type alias DebugInfo =
    {   clientId : String
    ,   numGamesCreatedAtConnectionTime : Int
    }

type alias NewGame game = Random.Seed -> List BasicTypes.PlayerId -> ( game, Random.Seed )

-- "base class"
type alias GameInfo game =
    -- for now, fixed number per game
    {   playersPerGame : Int
    ,   newGame : NewGame game
    }

{--

when switching to session ids for players ids, need to maintain a map of player
ids to client ids, updating whenever a new one connects

could even update multiple clients per player

--}

type alias GameMap game =
    {   waitingPlayers : List BasicTypes.PlayerId
    ,   nextGameId : GameId
    ,   playersToGames : Dict BasicTypes.PlayerId GameId
    ,   games : Dict GameId game
    ,   randomSeed : Random.Seed
    ,   gameInfo : GameInfo game
    }

newGameMap : Int -> Random.Seed -> NewGame game -> GameMap game
newGameMap playersPerGame seed newGame =
    {   waitingPlayers = []
    ,   nextGameId = 0
    ,   playersToGames = Dict.empty
    ,   games = Dict.empty
    ,   randomSeed = seed
    ,   gameInfo =
        {   playersPerGame = playersPerGame
        ,   newGame = newGame
        }
    }

queueForGame : BasicTypes.PlayerId -> GameMap game -> ( GameMap game, Maybe game )
queueForGame playerId gameMap =

    let
        waitingPlayers = playerId :: gameMap.waitingPlayers
    in

    -- for robustness, probably want to recurse while excess players
    if List.length waitingPlayers == gameMap.gameInfo.playersPerGame then
        let
            ( game, nextSeed ) = gameMap.gameInfo.newGame gameMap.randomSeed waitingPlayers
        in
        (   {   gameMap
            |   waitingPlayers = []
            ,   nextGameId = gameMap.nextGameId + 1
            ,   playersToGames = Dict.insert playerId gameMap.nextGameId gameMap.playersToGames
            ,   games = Dict.insert gameMap.nextGameId game gameMap.games
            ,   randomSeed = nextSeed
            }
        ,   Just game
        )
    else
        ( {gameMap | waitingPlayers = waitingPlayers}, Nothing )

sendToFrontend : (BasicTypes.PlayerId -> message -> Cmd backendMsg) -> GameMap game -> BasicTypes.PlayerId -> message -> Cmd backendMsg
sendToFrontend sendFunc gameMap playerId message =
    -- will look up client id(s)
    sendFunc playerId message
