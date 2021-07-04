module Cards exposing (
    RandomRequest, RandomResponse, Card, Deck, 
    deal, getShuffledPack
    )

import Array exposing (Array)
import List.Extra exposing (andThen)
import Random
import Random.List exposing (shuffle)

suits = 4
cards = 13
regularPack = List.range 0 (suits - 1)
    |> andThen 
        (\suit -> 
            List.range 0 (cards - 1)
                |> andThen
                    (\card -> [( suit, card )])
        )


getShuffledPack = shuffle regularPack

type alias Card = ( Int, Int )
type alias Deck = List Card

type alias RandomResponse = Int
type alias RandomRequest = Int --Random.Generator RandomResponse

---- utils
mapArrayElement : (a -> a) -> Int -> Array a -> Array a
mapArrayElement f index arr =
    Array.get index arr
        |> Maybe.map (\el -> Array.set index (f el) arr)
        |> Maybe.withDefault arr

-- deal up to N cards for each player (could add offset for first player to deal to)
deal : Int -> Array Deck -> Deck -> ( Array Deck, Deck )
deal perPlayer playerDecks drawPile = 
    let
        numPlayers = Array.length playerDecks
        impl : Int -> Int -> Array Deck -> Deck -> ( Array Deck, Deck ) 
        impl remaining playerIndex p d =
            case ( remaining, d ) of
                ( 0, _ ) -> 
                    ( p, d )

                ( _, [] ) -> 
                    ( p, d )

                ( _, nextCard :: draw ) ->
                    impl
                        (remaining - 1)
                        (remainderBy numPlayers (playerIndex + 1))
                        (mapArrayElement ((::) nextCard) playerIndex p)
                        draw

        ( drawnDecks, remainingPile ) = impl (perPlayer * numPlayers) 0 playerDecks drawPile
    in
    ( Array.map List.sort drawnDecks, remainingPile )
