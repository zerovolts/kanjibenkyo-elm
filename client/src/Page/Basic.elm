module Page.Basic exposing
    ( BlockType(..)
    , button
    , buttonCancel
    , buttonDisabled
    , charBlock
    , hr
    , radioButton
    )

import Color
import Element as E exposing (Element, el, link, px, text)
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
            , E.width (px 48)
            , E.height (px 48)
            , Border.rounded 5
            , Font.size 24
            , E.pointer
            ]
            { url = "/kana/" ++ char
            , label =
                el
                    [ E.centerX
                    , E.centerY
                    ]
                    (text char)
            }
        )


hr : Element Msg
hr =
    el
        [ E.width E.fill
        , E.height (px 3)
        , Background.color Color.backgroundDark
        , Border.rounded 5
        ]
        E.none


button : { label : Element Msg, onPress : Maybe Msg } -> Element Msg
button =
    Input.button
        [ Background.color Color.orange
        , Font.color Color.white
        , Border.rounded 5
        , E.paddingXY 12 8
        ]


buttonCancel : { label : Element Msg, onPress : Maybe Msg } -> Element Msg
buttonCancel =
    Input.button
        [ Background.color Color.red
        , Font.color Color.white
        , Border.rounded 5
        , E.paddingXY 12 8
        ]


buttonDisabled : Element Msg -> Element Msg
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


radioButton : Bool -> { label : Element Msg, onPress : Maybe Msg } -> Element Msg
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
