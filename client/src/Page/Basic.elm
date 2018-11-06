module Page.Basic exposing (BlockType(..), button, charBlock, hr, radioButton)

import Color
import Element exposing (Element, el, link, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Msg exposing (Msg)


type BlockType
    = WhiteBlack
    | Faded


charBlock : BlockType -> String -> Element Msg
charBlock blockType char =
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
            , Element.width (px 48)
            , Element.height (px 48)
            , Border.rounded 5
            , Font.size 24
            , Element.pointer
            ]
            { url = "/kana/" ++ char
            , label =
                el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (text char)
            }
        )


hr : Element Msg
hr =
    el
        [ Element.width Element.fill
        , Element.height (px 3)
        , Background.color Color.backgroundDark
        , Border.rounded 5
        ]
        Element.none


button : { label : Element Msg, onPress : Maybe Msg } -> Element Msg
button =
    Input.button
        [ Background.color Color.orange
        , Font.color Color.white
        , Border.rounded 5
        , Element.paddingXY 12 8
        ]


radioButton : Bool -> { label : Element Msg, onPress : Maybe Msg } -> Element Msg
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
