module Backend exposing (..)

import Array exposing (Array)
import Html
import Lamdera
import Random
import Types exposing (..)

import Cards exposing (getShuffledPack)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { waitingPlayers = [], games = Array.empty }
    , Cmd.none
    )

gameBroadcast : Game -> ToFrontend -> List (Cmd BackendMsg)
gameBroadcast (WipGame players _) payload =
    List.map (\p -> Lamdera.sendToFrontend p payload) players


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnection player ->
            let

                queue = player :: model.waitingPlayers

                sendDebugInfo games = 
                    Lamdera.sendToFrontend player <| InitDebugInfo
                        <| DebugInfo ("id: " ++ player) (Array.length games)

            in

            -- 1 means 2 players per game
            if List.length model.waitingPlayers == 1 then
                let
                    newGame = WipGame queue Array.empty
                    games = Array.push newGame model.games

                    deckRequest = Random.generate (GameDeck <| Array.length model.games) getShuffledPack
                in
                ( { model
                  | waitingPlayers = []
                  , games = games
                  }

                , Cmd.batch [ deckRequest, sendDebugInfo games ]
                ) 

            else
                ( { model
                  | waitingPlayers = queue
                  }
                , sendDebugInfo model.games
                )

        
        GameDeck id deck ->
            case getGame id model of
                Just ((WipGame players _) as game) ->
                    let
                        numPlayers = List.length players

                        ( playerDecks, drawPile ) = Cards.deal 5 (Array.repeat numPlayers []) deck
                        decks = Array.push drawPile playerDecks

                        -- will filter all decks in a generic way. for now just use index
                        blah: Int -> Cards.Deck -> Cmd BackendMsg
                        blah index playerDeck = Lamdera.sendToFrontend
                            (Maybe.withDefault "!" <| List.head (List.drop index players))
                            (OnGameStateChanged InProgress playerDeck)
                    in
                    ( { model
                      | games = updateGame id (WipGame players decks) model
                      }
                    , Cmd.batch
                        <| Array.toList
                        <| Array.indexedMap blah playerDecks
-- indexedMap : (Int -> a -> b) -> Array a -> Array b



                    )


                _ ->
                    ( model, Cmd.none )



updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

-- onConnect : (SessionId -> ClientId -> backendMsg) -> Sub backendMsg
subscriptions model = Lamdera.onConnect (\s -> \c -> OnConnection c)
