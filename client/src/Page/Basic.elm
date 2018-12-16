module Page.Basic exposing
    ( BlockType(..)
    , button
    , buttonCancel
    , buttonDisabled
    , charBlock
    , hr
    , kanaBlock
    , kanjiBlock
    , radioButton
    , tag
    )

import Color
import Element as E exposing (Color, Element, el, link, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed


type BlockType
    = WhiteBlack
    | Faded


kanjiBlock : String -> Element msg
kanjiBlock kanji =
    charBlock "/kanji/" WhiteBlack kanji


kanaBlock : String -> Element msg
kanaBlock kana =
    charBlock "/kana/" WhiteBlack kana


charBlock : String -> BlockType -> String -> Element msg
charBlock urlPrefix blockType char =
    let
        ( backgroundColor, fontColor ) =
            case blockType of
                WhiteBlack ->
                    ( Color.white, Color.textLight )

                Faded ->
                    ( Color.backgroundDark, Color.white )
    in
    Keyed.el []
        ( char
        , link
            [ Background.color backgroundColor
            , Font.color fontColor
            , E.width (px 48)
            , E.height (px 48)
            , E.pointer
            , Border.rounded 5
            , Font.size 24
            ]
            { url = urlPrefix ++ char
            , label =
                el
                    [ E.centerX
                    , E.centerY
                    ]
                    (text char)
            }
        )


hr : Element msg
hr =
    el
        [ E.width E.fill
        , E.height (px 3)
        , Background.color Color.backgroundDark
        , Border.rounded 5
        ]
        E.none


button : { label : Element msg, onPress : Maybe msg } -> Element msg
button =
    Input.button
        [ Background.color Color.orange
        , Font.color Color.white
        , Border.rounded 5
        , E.paddingXY 12 8
        ]


buttonCancel : { label : Element msg, onPress : Maybe msg } -> Element msg
buttonCancel =
    Input.button
        [ Background.color Color.red
        , Font.color Color.white
        , Border.rounded 5
        , E.paddingXY 12 8
        ]


buttonDisabled : Element msg -> Element msg
buttonDisabled label =
    Input.button
        [ Background.color Color.backgroundDark
        , Font.color Color.white
        , Border.rounded 5
        , E.paddingXY 12 8
        ]
        { label = label
        , onPress = Nothing
        }


radioButton : Bool -> { label : Element msg, onPress : Maybe msg } -> Element msg
radioButton selected =
    let
        optionalAttributes =
            if selected then
                [ Background.color Color.orangeDark
                ]

            else
                [ Background.color Color.orange
                , E.moveUp 4
                ]
    in
    Input.button
        ([ Font.color Color.white
         , E.paddingXY 16 10
         , Border.rounded 5
         ]
            ++ optionalAttributes
        )


tag : Color -> Color -> Element msg -> Element msg
tag background foreground =
    el
        [ Background.color background
        , Font.color foreground
        , Font.size 14
        , E.padding 4
        , Border.rounded 5
        ]
