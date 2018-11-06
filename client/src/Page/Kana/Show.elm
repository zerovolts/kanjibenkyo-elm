module Page.Kana.Show exposing (view)

import Color
import Dict exposing (Dict)
import Element exposing (Element, column, el, px, row, text)
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
        [ Element.width Element.fill
        , Element.padding 64
        ]
        [ row
            [ Element.centerX
            , Element.spacing 64
            ]
            [ el
                [ Border.rounded 5
                , Background.color Color.white
                , Element.width (px 128)
                , Element.height (px 128)
                , Font.size 80
                ]
                (el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (text (String.fromChar kanaChar))
                )
            , row
                [ Element.spacing 12
                , Element.alignTop
                ]
                (List.map (charBlock WhiteBlack)
                    [ String.fromChar kana.hiragana, String.fromChar kana.katakana, kana.romaji ]
                )
            ]
        ]
