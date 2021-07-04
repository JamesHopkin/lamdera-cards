module Backend exposing (..)

import Array exposing (Array)
import Dict
import Html
import Lamdera
import Random

import Bezique
import Cards exposing (getShuffledPack)
import Game
import Types


cardsPerPlayer = 7
playersPerGame = 2

type alias Model =
    Types.BackendModel

{--
copy minimal amount of code per game for init/update and presumably
some messaging
--}

app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }

init : ( Model, Cmd Types.BackendMsg )
init = 
    (   { state = Nothing, other = "not initialised" }
    ,   Random.generate Types.InitialSeed Random.independentSeed
    )

update : Types.BackendMsg -> Model -> ( Model, Cmd Types.BackendMsg )
update msg model =
    case msg of
        Types.GameBackend gameMessage ->
            Types.backendUpdate gameMessage model

        Types.AdminBackend ->
            ( model, Cmd.none )

        Types.OnConnection player ->
            case model.state of
                Just ({games} as state) ->
                    let
                        sendFunc playerId message =
                            message
                                |> Types.BeziqueToFrontend
                                |> Types.GameToFrontend
                                |> Lamdera.sendToFrontend playerId
                    -- automatically queue for Bezique for now
                        ( bezique, cmd ) = Bezique.requestGame
                            (Types.BeziqueBackend >> Types.GameBackend)
                            sendFunc
                            player games.bezique
                    in
                    (   {   model
                        |   state = Just
                            {   state
                            |   games =
                                {   games
                                |   bezique = bezique
                                }
                            }
                        }
                    ,   Cmd.batch
                            [   cmd
                            ,   Lamdera.sendToFrontend player
                                    <| Types.InitDebugInfo
                                    <| Game.DebugInfo ("id: " ++ player) (Dict.size games.bezique.games)
                            ]
                    )

                _ ->
                    ( model, Cmd.none )

        Types.InitialSeed seed ->
            let
                ( beziqueSeed, seed0 ) = Random.step Random.independentSeed seed
            in
            (   {   state = Just
                    {   games = 
                        {   bezique = Bezique.init beziqueSeed
                        ,   twoOhFourEight = ()
                        }
                    }
                ,   other = "got seed!"
                }
            ,   Cmd.none
            )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> Types.ToBackend -> Model -> ( Model, Cmd Types.BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Types.GameToBackend gameMessage ->
            Types.updateFromFrontend gameMessage model

        Types.BackendForceInit ->
            ( model, Random.generate Types.InitialSeed Random.independentSeed )

-- onConnect : (SessionId -> ClientId -> backendMsg) -> Sub backendMsg
subscriptions model = Lamdera.onConnect (\s -> \c -> Types.OnConnection c)
