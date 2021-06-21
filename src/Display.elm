module Display exposing (drawCards)

import Html.Styled exposing (..) 
--import Html.Attributes exposing (..) 
import Html.Styled.Attributes exposing (css, src)

import Css exposing (..)
import Css.Transitions exposing (..)

import Cards exposing ( Deck )

getSuit n = 
    case n of
        0 -> "spades_"
        1 -> "hearts_"
        2 -> "clubs_"
        3 -> "diamonds_"
        _ -> "!"

getRank n = 
    case n of 
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        10 -> "10"
        11 -> "jack"
        12 -> "queen"
        13 -> "king"
        14 -> "ace"
        _ -> "!"

cardsRoot = "https://tekeye.uk/playing_cards/images/svg_playing_cards/"

--drawCards : Deck -> Html msg
drawCards cards =  
    let
        addCard x y angle card = 
            div [ css 
                    [ position absolute
                    , Css.left (px x)
                    , Css.top (px y)
                    , Css.transform (rotate (deg angle))
                    , Css.hover
                        [ Css.top <| px <| y - 20
                        ]
                    , transition
                        [ Css.Transitions.top 200
                        ]
                    ]
                ]
                [ img [src (cardsRoot ++ "fronts/" ++ card ++ ".svg")] []
                ]
        indexer index ( s, n ) =
            let
                factor = toFloat index
            in
            addCard
                (50 + factor * 15)
                (60 + factor * 10)
                (factor * 10 - 15) (getSuit s ++ getRank (n + 2))
    in
    toUnstyled <| div [css [position relative]] <| List.indexedMap indexer cards
