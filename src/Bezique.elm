module Bezique exposing (..)

import Cards exposing ( Card, Deck )

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

type alias Game =
    { cards:
            { pile: Deck
            , players: ( PlayerCards, PlayerCards )
            }
    , turn: Turn
    }

type alias FrontendGame =
    { pile: Int
    , self: PlayerCards
    , other: OtherPlayerCards
    }

-- need update as well
getPlayers: Int -> { a | players: ( PlayerCards, PlayerCards ) } -> ( PlayerCards, PlayerCards )
getPlayers index {players} =
    case ( index, players ) of
        ( 0, _ ) ->
            players

        ( _, ( first, second ) ) ->
            ( second, first )


toFrontend : Int -> Game -> FrontendGame
toFrontend index game =
    let
        ( self, other ) = getPlayers index game.cards
    in
    {   pile = List.length game.cards.pile
    ,   self = self
    ,   other =
        {   hand = List.length other.hand
        ,   tricks = List.length other.tricks
        ,   melds = other.melds
        }
    }
