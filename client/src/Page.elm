module Page exposing (navBar, navLink, navLinks, titleText, view)

import Browser exposing (Document)
import Color
import Element
    exposing
        ( Element
        , column
        , el
        , link
        , px
        , row
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (node)
import Html.Attributes exposing (href, rel)
import Msg exposing (Msg)


view : Element Msg -> Document Msg
view content =
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
            (column
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ navBar
                    [ titleText
                    , navLinks
                    ]
                , el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    ]
                    content

                -- , el
                --     [ Element.width Element.fill
                --     , Element.height (px 24)
                --     , Background.color Color.backgroundDark
                --     ]
                --     (el [ Element.paddingXY 24 0, Element.centerY ]
                --         (text "hello")
                --     )
                ]
            )
        ]
    }


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


titleText =
    link
        [ Element.height Element.fill
        ]
        { url = "/"
        , label =
            row
                [ Font.size 26
                , Element.pointer
                ]
                [ el [] (text "漢字勉強")
                , el [ Font.color Color.white ] (text "kanjibenkyō")
                ]
        }


navLinks =
    row
        [ Font.size 20
        , Element.centerY
        , Font.color Color.white
        , Element.height Element.fill
        , Element.pointer
        ]
        [ navLink "Kana" "/kana", navLink "Kanji" "/kanji", navLink "Words" "/words" ]


navLink label url =
    link
        [ Element.mouseOver [ Background.color Color.orangeDark ]
        , Element.height Element.fill
        , Element.paddingXY 24 0
        ]
        { url = url
        , label = el [ Element.centerY ] (text label)
        }
