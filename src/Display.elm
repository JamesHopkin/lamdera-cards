module Display exposing (drawCards)


import Html.Styled exposing (..) 
--import Html.Attributes exposing (..) 
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (..) 

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
drawCards cards selected clickMsg =  
    let
        commonCss x y angle =
            [ position absolute
            , Css.left (px x)
            , Css.top (px y)
            , Css.transform (rotate (deg angle))
            ]

        additionalUnselected y =
            [ Css.hover
                [ Css.top (px (y - 20))
                ]
            , transition
                [ Css.Transitions.top 200
                ]
            ]

        indexer index ( s, n ) =
            let
                factor = toFloat index
                x = 50 + factor * 15
                ybase = 60 + factor * 10
                angle = factor * 10 - 15
                card = getSuit s ++ getRank (n + 2)

                y = if index == selected then
                        ybase

                    else
                        ybase + 40

                withCss = commonCss x y angle ++ (
                    if index == selected then
                        []

                    else
                        additionalUnselected y
                    )
            in
            div [ css withCss
                , onClick (clickMsg index)
                ]
                [ img [src (cardsRoot ++ "fronts/" ++ card ++ ".svg")] []
                ]
    in
    toUnstyled
        <| div
            [ css [position relative]
            , css [Css.backgroundColor (rgb 0 192 0)]
            ]
        <| (::) (div [ css
            [ position absolute
            , Css.width (px 640), Css.height (px 640)
            , Css.backgroundColor (rgb 0 160 0)
            ]] [])
        <| List.indexedMap indexer cards
