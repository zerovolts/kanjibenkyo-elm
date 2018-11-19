module Page.Word.Inflector exposing (view)

import Color
import Dict exposing (Dict)
import Element as E exposing (Element, column, el, px, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Kana exposing (Kana, hiraganaToRomaji)
import Msg exposing (Msg(..))
import Page.Basic exposing (button, buttonCancel, buttonDisabled, hr, radioButton)
import Word exposing (InflectedWord)


view : Dict Char Kana -> InflectedWord -> Element Msg
view kanaDict word =
    let
        wordChangeRadio =
            row
                [ E.spacing 1
                , Background.color Color.orangeDark
                , Border.rounded 5
                ]
                (List.map
                    (\basicWord -> radioButton (word.word == basicWord) { label = text basicWord.root, onPress = Just (ChangeCurrentWord basicWord) })
                    [ Word.default, Word.defaultGodan ]
                )

        removeButton =
            if word.intents == [] then
                buttonDisabled (text "Remove Last")

            else
                buttonCancel
                    { label = text "Remove Last"
                    , onPress = Just RemoveInflection
                    }

        clearButton =
            if word.intents == [] then
                buttonDisabled (text "Clear")

            else
                buttonCancel
                    { label = text "Clear"
                    , onPress = Just ClearInflections
                    }

        inflectionButtons =
            List.map
                (\intent ->
                    if Word.wordToValidIntents word |> List.member intent then
                        button
                            { label = text (Word.intentToString intent)
                            , onPress = Just (InflectWord intent)
                            }

                    else
                        buttonDisabled (text (Word.intentToString intent))
                )
                Word.allIntents
    in
    column
        [ E.width (px 800), E.centerX, E.padding 64, E.spacing 24 ]
        [ row [ E.width E.fill, E.spaceEvenly ] [ el [ Font.size 30 ] (text "Word Inflection Lab"), wordChangeRadio ]
        , hr
        , el [ Font.size 40 ]
            (text (Word.toString word))
        , el []
            (text (hiraganaToRomaji kanaDict (Word.toKanaString word)))
        , hr
        , el [ E.spacing 8 ]
            (text
                (if word.intents == [] then
                    "Dictionary Form (No Inflections)"

                 else
                    String.join " → " (List.map Word.intentToString (List.reverse word.intents))
                )
            )
        , wrappedRow [ E.spacing 8 ]
            (clearButton :: removeButton :: inflectionButtons)
        ]
