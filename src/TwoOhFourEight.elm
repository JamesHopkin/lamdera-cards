module TwoOhFourEight exposing (..)

import Browser
import Browser.Events as Events

import Css exposing (..)

import Dict exposing (Dict)

import Html
import Html.Styled exposing (..) 
import Html.Styled.Attributes exposing (css)

import Html.Events exposing (preventDefaultOn)
import Json.Decode as Json

import List.Extra as List

import Json.Decode as Decode

import Random

keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

slide2 ma mb =
    case ( ma, mb ) of
        ( _, Nothing ) -> ( Nothing, ma )
        ( Just a, Just b ) ->
            if a == b then ( Nothing, Just (a * 2) ) else ( ma, mb )
        _ -> ( ma, mb )

slide4List : List (Maybe Int) -> List (Maybe Int)
slide4List rowList =
    let
        impl : List Int -> List Int
        impl l =
            case l of
                a :: b :: c :: d :: [] ->
                    if c == d then
                        if a == b then [ a * 2, c * 2 ]
                        else [ a, b, c * 2 ]
                    else if b == c then
                        [ a, b * 2, d ]
                    else if a == b then
                        [ a * 2, c, d ]
                    else
                        [ a, b, c, d ]

                a :: b :: c :: [] ->
                        if b == c then
                            [ a, b * 2 ]
                        else if a == b then
                            [ a * 2, c ]
                        else
                            [ a, b, c ]

                a :: b :: [] ->
                        if a == b then
                            [ a * 2 ]
                        else
                            [ a, b ]

                _ ->
                    l

        combined = impl (List.filterMap identity rowList)
    in
    (List.repeat (4 - List.length combined) Nothing)
        ++ (List.map Just combined)


type alias Board = Dict ( Int, Int ) Int
slideLine fullLineDef board =
    let
        newList = slide4List <| List.map (\k -> Dict.get k board) fullLineDef

        foldFunc : Maybe Int -> ( List ( Int, Int ), Board ) -> ( List ( Int, Int ), Board )
        foldFunc maybeContents ( lineDef, d ) =
            case lineDef of
                k :: lineDefTail ->
                    (   lineDefTail
                    ,   maybeContents
                        |>  Maybe.map (\contents -> Dict.insert k contents d) 
                        |>  Maybe.withDefault d
                    )
                _ ->
                    ( [], Dict.empty ) 

        ( _, newDict ) = List.foldl foldFunc ( fullLineDef, Dict.empty ) newList
    in
    newDict

type Dir = Up | Right | Down | Left

slideBoard : Dir -> Board -> Board
slideBoard dir board =
    let
        indexer major minor =
            case dir of
                Right -> ( major, minor )
                Down -> ( minor, major )
                Left -> ( major, 3 - minor )
                Up -> ( 3 - minor, major )

        foldFunc : Int -> Board -> Board
        foldFunc row d =
            List.range 0 3
            |> List.map (indexer row)
            |> \lineDef -> slideLine lineDef board
            |> Dict.union d
    in
    List.range 0 3
        |> List.foldl foldFunc Dict.empty


commonCss =
    [   display inlineBlock
    ,   minWidth (px 43)
    ,   paddingTop (px 11)
    ,   paddingBottom (px 9)
    ,   margin (px 4)
    ,   fontSize (px 22)
    ,   fontWeight bold
    ,   textAlign center
    ,   fontFamilies ["roboto", "sans-serif"]
    ]

view model = 
    let
        render n =
            let
                fg = hex (if n <= 4 then "776e65" else "f9f6f2")
                bg = hex (
                    case n of
                        512 -> "edc950"
                        256 -> "edcc62"
                        128 -> "edd073"
                        64 -> "f75f3b"
                        32 -> "fc7c5f"
                        16 -> "f69664"
                        8 -> "f3b27a"
                        4 -> "eee1c9"
                        _ -> "eee4da"
                    )

            in
            span [css (commonCss ++ 
                    [   color fg
                    ,   backgroundColor bg
                    ])
                ]
                [n |> String.fromInt |> text]

        cellFunc y x =
            let
                defaultSpan = span
                    [css <| commonCss ++
                        [   backgroundColor (hex "ccc0b4")
                        ,   minHeight (px 35)
                        ,   position relative
                        ,   top (px -3)
                        ,   paddingTop (px 0)
                        ,   verticalAlign middle
                        ]
                    ]
                    []
            in
            Dict.get ( y, x ) model.board
                |> Maybe.map render
                |> Maybe.withDefault defaultSpan

        rowFunc rowIndex =
            List.range 0 3
                |> List.map (cellFunc rowIndex)
                |> div []

    in
    List.range 0 3
        |> List.map rowFunc
        |> div [ css 
            [   backgroundColor (hex "bbada0")
            ,   minHeight (px 51)
            ]]
        |> toUnstyled
        |> List.singleton
        |> Html.div
            [   decoder TouchStart "targetTouches" "touchstart"
            ,   decoder TouchEnd "changedTouches" "touchend"
            ]

type alias Model =
    { board: Board
    , seed: Random.Seed
    , touchStart: Maybe ( Float, Float )
    }

type Msg
    = KeyDown String
    | TouchStart ( Float, Float )
    | TouchEnd ( Float, Float )
    | InitialSeed Random.Seed

dictsEqual d1 d2 =
    if Dict.size d1 /= Dict.size d2 then
        False

    else
        Dict.toList d1
            |> List.all (\( k, v1 ) ->
                Dict.get k d2
                    |> Maybe.map (\v2 -> v1 == v2)
                    |> Maybe.withDefault False
            )

decoder msg attrName event =
    Json.map2 Tuple.pair
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        |>  (let
                dummy = Debug.log "decoder" event
            in
            Json.at [attrName, "0"])
        |>  Json.map (\attr -> ( msg attr, False ))
        |>  preventDefaultOn event


update msg model =
    let
        handle dir =
            let
                afterSlide = slideBoard dir model.board
            in
            { model
            | board = afterSlide
            }
            |> (if dictsEqual model.board afterSlide then
                    identity
                else
                    dropInRandomLocation
                )

    in
    case msg of
        KeyDown k ->
            case k of 
                "w" -> handle Up
                "a" -> handle Left
                "s" -> handle Down
                "d" -> handle Right

                _ -> model

        TouchStart p ->
            {   model
            |   touchStart = Just p
            }

        TouchEnd ( x, y ) ->
            let
                newModel = 
                    case model.touchStart of
                        Just start ->
                            let
                                ( startX, startY ) = start
                                ( dx, dy ) = ( x - startX, y - startY )
                            in
                            handle <|
                                if dx < dy then
                                    if dx < -dy then
                                        Left
                                    else
                                        Down
                                else
                                    if dx < -dy then
                                        Up
                                    else
                                        Right

                        Nothing ->
                            model
            in
            {   newModel
            |   touchStart = Nothing
            }

        InitialSeed seed ->
            {   model
            |   seed = seed
            } |> dropInRandomLocation

emptyLocations board =
    List.range 0 3
        |> List.andThen (\y ->
            List.range 0 3
                |> List.andThen (\x ->
                    if Dict.member ( y, x ) board then
                        []

                    else
                        [ ( y, x ) ]
                    )
            )

dropInRandomLocation model =
    let
        empty = emptyLocations model.board

        gen = Random.pair
            (Random.int 0 (List.length empty - 1))
            (Random.int 0 9 )

        ( ( randomIndex, rand10 ), seed ) = Random.step gen model.seed

        board =
            case List.head (List.drop randomIndex empty) of
                Just loc ->
                    Dict.insert loc (if rand10 == 0 then 4 else 2) model.board

                _ ->
                    model.board
    in
    {   model
    |   board = board
    ,   seed = seed
    }

init _ = 
    (   {   board = Dict.empty
        ,   seed = Random.initialSeed 0
        ,   touchStart = Nothing
        }
    ,   Random.generate InitialSeed Random.independentSeed
    )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg -> \model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Events.onKeyDown (Decode.map KeyDown keyDecoder)
        }