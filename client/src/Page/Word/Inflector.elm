module Page.Word.Inflector exposing (view)

import Element as E exposing (Element, column, el, row, text)
import Element.Font as Font
import Msg exposing (Msg(..))
import Page.Basic exposing (button, hr)
import Word exposing (InflectedWord)


view : InflectedWord -> Element Msg
view word =
    column
        [ E.width E.fill, E.centerX, E.padding 48, E.spacing 24 ]
        [ el [ Font.size 40, E.centerX ] (text (Word.toString word))
        , row [ E.spacing 8, E.centerX ] (List.map (Word.intentToString >> text) (List.reverse word.intents))
        , row [ E.spacing 8, E.centerX ]
            (button
                { label = text "-"
                , onPress = Just RemoveInflection
                }
                :: List.map
                    (\intent ->
                        button
                            { label = text (Word.intentToString intent)
                            , onPress = Just (InflectWord intent)
                            }
                    )
                    (Word.wordToValidIntents word)
            )
        ]
