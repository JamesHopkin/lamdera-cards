module Bezique exposing (..)

import Array
import Dict
import Html exposing (Html)
import Random

import BasicTypes
import Cards exposing ( Card, Deck )
import Display
import Game

type Phase
    = Choosing
    | PlacedTrick ( Card, Card ) -- will have a button to claim trick

type Turn = Turn Phase Int  -- 0 or 1

type alias PlayerCards =
    {   hand: Deck
    ,   tricks: List ( Card, Card )
    ,   melds: List Deck
    }

type alias OtherPlayerCards =
    {   hand: Int
    ,   tricks: Int
    ,   melds: List Deck
    }

type BackendGameMsg =
    Noop

type alias BackendPlayerState =
    {   playerId: BasicTypes.PlayerId
    ,   cards: PlayerCards
    }

type alias BackendGame =
    {   cards:
        { pile: Deck
        , players: ( BackendPlayerState, BackendPlayerState )
        }
    ,   turn: Turn
    }

type alias FrontendGame =
    { pile: Int
    , self: PlayerCards
    , other: OtherPlayerCards
    }

type alias FrontendModel =
    { game: FrontendGame
    , selected: Int
    }

init : Random.Seed -> BackendModel
init initialSeed =
    let
        newGame : Random.Seed -> List BasicTypes.PlayerId -> ( BackendGame, Random.Seed )
        newGame seed playerIdList =
            let
                emptyCards = {tricks = [], melds = [], hand = []}

                -- should never be used
                emptyResult =
                    (   {   cards =
                            {   pile = []
                            ,   players = ( {cards = emptyCards, playerId = ""}, {cards = emptyCards, playerId = ""} )
                            }
                        ,   turn = Turn Choosing 0
                        }
                    ,   Random.initialSeed 0
                    )
            in
            case playerIdList of
                player0 :: player1 :: [] ->
                    let
                        ( fullDeck, nextSeed ) = Random.step Cards.getShuffledPack seed
                        ( handArray, remainingDeck ) = Cards.deal 8
                                                    (Array.fromList [[], []])
                                                    fullDeck

                    in
                    case ( Array.get 0 handArray, Array.get 1 handArray ) of
                        ( Just hand0, Just hand1 ) ->
                            (   {   cards =
                                    {   pile = remainingDeck
                                    ,   players =
                                        (   { cards = {emptyCards | hand = hand0}
                                            , playerId = player0
                                            }
                                        ,   { cards = {emptyCards | hand = hand1}
                                            , playerId = player1
                                            }
                                     
                                        )
                                    }
                                ,   turn = Turn Choosing 0
                                }
                            ,   nextSeed
                            )


                        _ ->
                            emptyResult
                _ ->
                    emptyResult
    in
    Game.newGameMap 2 initialSeed newGame


frontendInit =
    { game =
      { pile = 0
      , self =
        { hand = []
        , tricks = []
        , melds = []
        }
      , other =
        { hand = 0
        , tricks = 0
        , melds = []
        }
      }
    , selected = -1
    }

backendGameUpdate msg message game = ( game, Cmd.none )

frontendUpdate : (FrontendMsg -> msg) -> FrontendMsg -> FrontendModel
                        -> ( FrontendModel, Cmd msg )
frontendUpdate msg message model = ( model, Cmd.none )

-- need update as well
getPlayers: Int -> ( BackendPlayerState, BackendPlayerState ) -> ( BackendPlayerState, BackendPlayerState )
getPlayers index players =
    case ( index, players ) of
        ( 0, _ ) ->
            players

        ( _, ( first, second ) ) ->
            ( second, first )

type FrontendMsg
    = Click Int

type ToFrontendMsg =
    FullUpdate FrontendGame

type ToBackendGameMsg =
    GameNoop -- e.g. play card

type alias SendFunc backendMsg = BasicTypes.PlayerId -> ToFrontendMsg -> Cmd backendMsg
sendToFrontend : SendFunc msg -> BackendModel -> BackendGame -> Int -> Cmd msg
sendToFrontend sendFunc model game playerIndex =
    let
        ( self, other ) = getPlayers playerIndex game.cards.players

        payload : FrontendGame
        payload =
            {   pile = List.length game.cards.pile
            ,   self = self.cards
            ,   other =
                {   hand = List.length other.cards.hand
                ,   tricks = List.length other.cards.tricks
                ,   melds = other.cards.melds
                }
            }
    in
    payload
        |> FullUpdate
        |> Game.sendToFrontend sendFunc model self.playerId


updateGameFromFrontend : (BackendMsg -> msg) -> ToBackendGameMsg -> BackendGame -> ( BackendGame, Cmd msg )
updateGameFromFrontend msg fromFrontendMessage game = ( game, Cmd.none )

--updateFrontend playerIndex 

requestGame : (BackendMsg -> msg) -> SendFunc msg -> BasicTypes.PlayerId -> BackendModel -> ( BackendModel, Cmd msg )
requestGame backendMsg sendFunc playerId model =
    let
        ( gameMap, maybeGame ) = Game.queueForGame playerId model

        cmds =
            case maybeGame of
                Just game ->
                    let
                        send = sendToFrontend sendFunc model game
                    in
                    Cmd.batch [send 0, send 1]

                _ ->
                    Cmd.none
    in
    ( gameMap, cmds )
    --case maybeGame of
    --    Just game ->
    --        -- send to both players
    --        Cmd.batch
    --            |> msg
    --            |> Lamdera.sendToFrontend 

view : (FrontendMsg -> msg) -> FrontendModel -> Html msg
view msg model = 
    Display.drawCards model.game.self.hand model.selected (Click >> msg)


---------------------
-- these two could be combined?

handleBackendUpdate : Game.GameId -> BackendModel -> ( BackendGame, Cmd msg )
                                                    -> ( BackendModel, Cmd msg )
handleBackendUpdate gameId model ( updatedGame, cmd ) =
    (   {   model
        |   games = Dict.insert gameId updatedGame model.games
        }
    ,   cmd
    )

wrapBackendGameUpdate : Game.GameId -> a -> BackendModel
                            -> (a -> BackendGame -> ( BackendGame, Cmd msg ))
                            -> ( BackendModel, Cmd msg )
wrapBackendGameUpdate gameId gameMessage model f =
    case Dict.get gameId model.games of
        Just game ->
            handleBackendUpdate gameId model
                <| f gameMessage game

        _ ->
            let
                dummy = Debug.log "unknown game" gameId
            in
            ( model, Cmd.none )

---------------------


backendUpdate : (BackendMsg -> msg) -> BackendMsg -> BackendModel -> ( BackendModel, Cmd msg )
backendUpdate msg backendMsg model = 
    case backendMsg of
        BackendGameUpdate gameId gameMessage ->
            wrapBackendGameUpdate gameId gameMessage model <| backendGameUpdate msg

updateFromBackend : (FrontendMsg -> msg) -> ToFrontendMsg -> FrontendModel -> ( FrontendModel, Cmd msg )
updateFromBackend msg fromBackendMsg model = 
    case fromBackendMsg of
        FullUpdate game ->
            ( {game = game, selected = -1}, Cmd.none )

updateFromFrontend : (BackendMsg -> msg) -> ToBackendMsg -> BackendModel -> ( BackendModel, Cmd msg )
updateFromFrontend msg fromFrontendMsg model = 
    case fromFrontendMsg of
        ToBackendGame gameId gameMessage ->
            wrapBackendGameUpdate gameId gameMessage model <| updateGameFromFrontend msg

        RequestGame ->
            ( model, Cmd.none )

-- seem like a lot of boilerplate: hoping it doesn't make too much difference
-- whether I wrap game map stuff in each game, or put it in Backend/Frontend.elm
-- once set up, should mean everything is in here
type alias BackendModel = Game.GameMap BackendGame

type ToBackendMsg
    = ToBackendGame Game.GameId ToBackendGameMsg
    | RequestGame 

type BackendMsg =
    BackendGameUpdate Game.GameId BackendGameMsg
