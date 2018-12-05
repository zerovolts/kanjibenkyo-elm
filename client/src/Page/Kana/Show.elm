module Page.Kana.Show exposing (view)

import Color
import Dict exposing (Dict)
import Element as E exposing (Element, column, el, px, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Kana exposing (Kana)
import Msg exposing (Msg)
import Page.Basic exposing (BlockType(..), charBlock)


view : Dict Char Kana -> Char -> Element Msg
view kanaDict kanaChar =
    let
        kana =
            Maybe.withDefault Kana.default (Dict.get kanaChar kanaDict)
    in
    column
        [ E.width E.fill
        , E.padding 64
        ]
        [ row
            [ E.centerX
            , E.spacing 64
            ]
            [ el
                [ Border.rounded 5
                , Background.color Color.white
                , E.width (px 128)
                , E.height (px 128)
                , Font.size 80
                ]
                (el
                    [ E.centerX
                    , E.centerY
                    ]
                    (text (String.fromChar kanaChar))
                )
            , row
                [ E.spacing 12
                , E.alignTop
                ]
                (List.map (charBlock WhiteBlack)
                    [ String.fromChar kana.hiragana
                    , kana.katakana
                        |> Kana.fromKatakana kanaDict
                        |> Maybe.withDefault Kana.default
                        |> .hiragana
                        |> String.fromChar
                    , kana.romaji
                    ]
                )
            ]
        ]
