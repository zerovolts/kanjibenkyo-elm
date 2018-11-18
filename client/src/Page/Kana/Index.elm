module Page.Kana.Index exposing (view)

import Color
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Kana exposing (Category(..), Kana)
import Msg exposing (Msg(..))
import Page.Basic exposing (BlockType(..), charBlock, hr, radioButton)


type alias Model =
    { kanaFilter : Category
    }


view : Dict Char Kana -> Category -> Element Msg
view kana kanaFilter =
    column
        [ Element.width Element.shrink
        , Element.centerX
        , Element.padding 64
        , Element.spacing 24
        ]
        [ row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text
                    ("Kana - "
                        ++ (String.fromInt <|
                                List.length <|
                                    Dict.toList kana
                           )
                    )
                )
            , kanaFilterGroup kanaFilter
            ]
        , hr
        , el [ Element.centerX ]
            (kanaGrid kana kanaFilter)
        ]


gridTemplate : List (List (Maybe Char))
gridTemplate =
    [ [ Just 'あ', Just 'い', Just 'う', Just 'え', Just 'お' ]
    , [ Just 'か', Just 'き', Just 'く', Just 'け', Just 'こ' ]
    , [ Just 'さ', Just 'し', Just 'す', Just 'せ', Just 'そ' ]
    , [ Just 'た', Just 'ち', Just 'つ', Just 'て', Just 'と' ]
    , [ Just 'な', Just 'に', Just 'ぬ', Just 'ね', Just 'の' ]
    , [ Just 'は', Just 'ひ', Just 'ふ', Just 'へ', Just 'ほ' ]
    , [ Just 'ま', Just 'み', Just 'む', Just 'め', Just 'も' ]
    , [ Just 'や', Nothing, Just 'ゆ', Nothing, Just 'よ' ]
    , [ Just 'ら', Just 'り', Just 'る', Just 'れ', Just 'ろ' ]
    , [ Just 'わ', Just 'ゐ', Nothing, Just 'ゑ', Just 'を' ]
    , [ Just 'ん', Nothing, Nothing, Nothing, Nothing ]
    ]


kanaGrid : Dict Char Kana -> Category -> Element Msg
kanaGrid kanaDict kanaFilter =
    let
        hydrateKana =
            \char ->
                case char of
                    Just c ->
                        let
                            kana =
                                Maybe.withDefault Kana.default (Dict.get c kanaDict)
                        in
                        charBlock WhiteBlack
                            (case kanaFilter of
                                Hiragana ->
                                    String.fromChar kana.hiragana

                                Katakana ->
                                    String.fromChar kana.katakana

                                Romaji ->
                                    kana.romaji
                            )

                    Nothing ->
                        charBlock WhiteBlack ""

        grid =
            List.reverse <|
                List.map
                    (\col ->
                        List.map hydrateKana col
                    )
                    gridTemplate
    in
    column [ spacing 12 ]
        [ row [ spacing 12 ]
            (List.map (charBlock Faded)
                [ "nn", "w", "r", "y", "m", "h", "n", "t", "s", "k", "-" ]
            )
        , row
            [ spacing 12
            , Element.onRight
                (column
                    [ Element.moveRight 12
                    , Element.spacing 12
                    ]
                    (List.map (charBlock Faded)
                        [ "a", "i", "u", "e", "o" ]
                    )
                )
            ]
            (List.map
                (\col ->
                    column
                        [ Element.spacing 12
                        ]
                        col
                )
                grid
            )
        ]


kanaFilterGroup : Category -> Element Msg
kanaFilterGroup kanaFilter =
    row
        [ Element.spacing 1
        , Background.color Color.orangeDark
        , Border.rounded 5
        ]
        [ radioButton (kanaFilter == Hiragana)
            { onPress = Just (ChangeKanaCategory Hiragana)
            , label = text "Hiragana"
            }
        , radioButton (kanaFilter == Katakana)
            { onPress = Just (ChangeKanaCategory Katakana)
            , label = text "Katakana"
            }
        , radioButton (kanaFilter == Romaji)
            { onPress = Just (ChangeKanaCategory Romaji)
            , label = text "Rōmaji"
            }
        ]