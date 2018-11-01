module View exposing (view)

import Browser exposing (Document)
import Color
import Dict
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , padding
        , px
        , row
        , spacing
        , text
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (node)
import Html.Attributes exposing (href, rel)
import Kana exposing (Category(..))
import Model exposing (Model)
import Msg exposing (Msg(..))


view : Model -> Document Msg
view model =
    let
        kanjiBlocks =
            column
                [ Element.width Element.fill
                ]
                [ navBar
                , column
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
                                                Dict.toList model.kana
                                       )
                                )
                            )
                        , kanaFilterGroup model.kanaFilter
                        ]
                    , hr
                    , el [ Element.centerX ]
                        (kanaGrid model.kana model.kanaFilter)
                    ]
                ]
    in
    { title = "kanjibenkyō"
    , body =
        [ node "link"
            [ href "https://fonts.googleapis.com/css?family=Rounded+Mplus+1c"
            , rel "stylesheet"
            ]
            []
        , node "link"
            [ href "https://fonts.googleapis.com/css?family=Varela+Round"
            , rel "stylesheet"
            ]
            []
        , Element.layout
            globalStyles
            kanjiBlocks
        ]
    }


kanaGridTemplate : List (List (Maybe Char))
kanaGridTemplate =
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
                        charBlock
                            (case kanaFilter of
                                Hiragana ->
                                    String.fromChar kana.hiragana

                                Katakana ->
                                    String.fromChar kana.katakana

                                Romaji ->
                                    kana.romaji
                            )

                    Nothing ->
                        charBlock ""

        grid =
            List.reverse <|
                List.map
                    (\col ->
                        List.map hydrateKana col
                    )
                    kanaGridTemplate
    in
    row [ Element.spacing 12 ]
        (List.map
            (\col ->
                column
                    [ Element.spacing 12
                    ]
                    col
            )
            grid
        )


hr =
    el
        [ Element.width Element.fill
        , Element.height (px 3)
        , Background.color Color.backgroundDark
        , Border.rounded 5
        ]
        Element.none


button =
    Input.button
        [ Background.color Color.orange
        , Font.color Color.white
        , Border.rounded 5
        , Element.paddingXY 12 8
        ]


radioButton selected =
    let
        optionalAttributes =
            if selected then
                [ Background.color Color.orangeDark
                ]

            else
                [ Background.color Color.orange
                , Element.moveUp 4
                ]
    in
    Input.button
        ([ Font.color Color.white
         , Element.paddingXY 16 10
         , Border.rounded 5
         ]
            ++ optionalAttributes
        )


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


navBar =
    row
        [ Background.color Color.orange
        , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
        , Border.color Color.orangeDark
        , Element.height (px 52)
        , Element.width Element.fill
        , Element.paddingXY 64 0
        , Element.spaceEvenly
        ]
        [ titleText
        , navLinks
        ]


titleText =
    row
        [ Font.size 26
        , Element.height Element.fill
        , Element.pointer
        ]
        [ el [] (text "漢字勉強")
        , el [ Font.color Color.white ] (text "kanjibenkyō")
        ]


navLinks =
    row
        [ Font.size 20
        , Element.centerY
        , Font.color Color.white
        , Element.height Element.fill
        , Element.pointer
        ]
        [ navLink "Kana", navLink "Kanji", navLink "Words" ]


navLink label =
    el
        [ Element.mouseOver [ Background.color Color.orangeDark ]
        , Element.height Element.fill
        , Element.paddingXY 24 0
        ]
        (el [ Element.centerY ] (text label))


globalStyles =
    [ Background.color Color.background
    , Font.family
        [ Font.typeface "Varela Round"
        , Font.typeface "Rounded Mplus 1c"
        , Font.sansSerif
        ]
    , Font.color Color.text
    , Font.size 16
    ]


charBlock char =
    el
        [ Background.color Color.white
        , Element.width (px 48)
        , Element.height (px 48)
        , Border.rounded 5
        , Font.size 24
        , Element.pointer
        ]
        (el
            [ Element.centerX
            , Element.centerY
            ]
            (text char)
        )
