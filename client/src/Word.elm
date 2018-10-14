module Word exposing (VerbCategory(..), Word, WordCategory(..), default, defaultGodan, getKanji, toNegative, toPast)

import Dict exposing (Dict)
import Kana
import Kanji exposing (Kanji)


type alias Word =
    { word : String
    , category : WordCategory
    , meanings : List String
    }


type WordCategory
    = Verb VerbCategory
    | Noun
    | Adjective
    | Adverb
    | Particle


type VerbCategory
    = Ichidan
    | Godan


default : Word
default =
    { word = "見る"
    , category = Verb Ichidan
    , meanings = [ "to see" ]
    }


defaultGodan : Word
defaultGodan =
    { word = "行く"
    , category = Verb Godan
    , meanings = [ "to go" ]
    }


getKanji : Dict Char Kanji -> Word -> List Kanji
getKanji allKanjiDict word =
    word.word
        |> String.toList
        |> List.filter (\char -> Kanji.isKanji char)
        |> List.foldl
            (\char kanjiList ->
                case Dict.get char allKanjiDict of
                    Just kanji ->
                        kanji :: kanjiList

                    Nothing ->
                        kanjiList
            )
            []


toPast : Word -> Maybe Word
toPast word =
    case word.category of
        Verb Ichidan ->
            Just
                { word
                    | word =
                        word.word
                            |> String.slice 0 -1
                            |> (\x -> String.append x "た")
                }

        Verb Godan ->
            Just word

        _ ->
            Nothing


toNegative : Word -> Maybe Word
toNegative word =
    case word.category of
        Verb Ichidan ->
            Just
                { word
                    | word =
                        word.word
                            |> String.slice 0 -1
                            |> (\x -> String.append x "ない")
                }

        Verb Godan ->
            let
                ( vowel, root ) =
                    Maybe.withDefault ( 'あ', "" ) (String.uncons (String.reverse word.word))
            in
            Just
                { word
                    | word =
                        String.reverse root ++ String.cons (Maybe.withDefault 'あ' (Kana.changeVowel vowel Kana.A)) "ない"
                }

        _ ->
            Nothing
