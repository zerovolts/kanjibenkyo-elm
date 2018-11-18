module Word exposing
    ( BasicWord
    , InflectedWord
    , WordCategory(..)
    , WordIntent(..)
    , default
    , defaultGodan
    , fromBasicWord
    , getKanji
    , intentToCategory
    , intentToString
    , pushIntent
    , removeIntent
    , toNegative
    , toPast
    , toString
    , wordToValidIntents
    )

import Dict exposing (Dict)
import Kana exposing (VowelCategory)
import Kanji exposing (Kanji)


type alias BasicWord =
    { root : String
    , category : WordCategory
    , meanings : List String
    }


default : BasicWord
default =
    { root = "見る"
    , category = Ichidan
    , meanings = [ "to see" ]
    }


defaultGodan : BasicWord
defaultGodan =
    { root = "話す"
    , category = Godan
    , meanings = [ "to go" ]
    }


getKanji : Dict Char Kanji -> BasicWord -> List Kanji
getKanji allKanjiDict word =
    word.root
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


inflect : WordIntent -> (BasicWord -> BasicWord)
inflect intent =
    case intent of
        Negative ->
            toNegative

        Past ->
            toPast

        Conjunctive ->
            toConjunctive

        Desire ->
            toDesire


toConjunctive : BasicWord -> BasicWord
toConjunctive word =
    let
        updateRoot =
            case word.category of
                Ichidan ->
                    replaceOkurigana 1 "た"

                Godan ->
                    godanRootToConjunctive

                IAdj ->
                    replaceOkurigana 1 "くて"

                _ ->
                    identity
    in
    { word
        | root = updateRoot word.root
        , category = TeForm
    }


godanRootToConjunctive : String -> String
godanRootToConjunctive root =
    let
        vowel =
            root
                |> String.reverse
                |> String.uncons
                |> Maybe.withDefault ( 'あ', "" )
                |> Tuple.first

        newVowel =
            if vowel == 'う' || vowel == 'つ' || vowel == 'る' then
                "って"

            else if vowel == 'ぬ' || vowel == 'む' || vowel == 'ぶ' then
                "んで"

            else if vowel == 'く' then
                "いて"

            else if vowel == 'ぐ' then
                "いで"

            else
                -- す
                "して"

        stem =
            String.slice 0 -1 root
    in
    stem ++ newVowel


toPast : BasicWord -> BasicWord
toPast word =
    let
        updateRoot =
            case word.category of
                Ichidan ->
                    replaceOkurigana 1 "た"

                Godan ->
                    \root ->
                        let
                            conjunctiveRoot =
                                godanRootToConjunctive root

                            -- should be て or で
                            lastChar =
                                conjunctiveRoot
                                    |> String.reverse
                                    |> String.uncons
                                    |> Maybe.withDefault ( 'て', "" )
                                    |> Tuple.first

                            conjunctiveStem =
                                String.slice 0 -1 conjunctiveRoot
                        in
                        if lastChar == 'て' then
                            conjunctiveStem ++ "た"

                        else
                            conjunctiveStem ++ "だ"

                IAdj ->
                    replaceOkurigana 1 "かった"

                _ ->
                    identity
    in
    { word
        | root = updateRoot word.root
        , category = TaForm
    }


toNegative : BasicWord -> BasicWord
toNegative word =
    let
        updateRoot =
            case word.category of
                Ichidan ->
                    replaceOkurigana 1 "ない"

                Godan ->
                    changeOkuriganaByVowel Kana.A "ない"

                IAdj ->
                    replaceOkurigana 1 "くない"

                _ ->
                    identity
    in
    { word
        | root = updateRoot word.root
        , category = IAdj
    }


toDesire : BasicWord -> BasicWord
toDesire word =
    let
        updateRoot =
            case word.category of
                Ichidan ->
                    replaceOkurigana 1 "たい"

                Godan ->
                    changeOkuriganaByVowel Kana.I "たい"

                _ ->
                    identity
    in
    { word
        | root = updateRoot word.root
        , category = IAdj
    }


replaceOkurigana : Int -> String -> (String -> String)
replaceOkurigana removeCount okurigana =
    String.slice 0 -removeCount
        >> (\stem -> String.append stem okurigana)


changeOkuriganaByVowel : VowelCategory -> String -> String -> String
changeOkuriganaByVowel newVowelCat okurigana root =
    let
        vowel =
            root
                |> String.reverse
                |> String.uncons
                |> Maybe.withDefault ( 'あ', "" )
                |> Tuple.first

        stem =
            String.slice 0 -1 root
    in
    stem
        ++ String.cons
            (Maybe.withDefault 'あ'
                (Kana.changeVowel vowel newVowelCat)
            )
            okurigana


type alias InflectedWord =
    { word : BasicWord
    , intents : List WordIntent
    }


type WordIntent
    = Negative
    | Desire
    | Past
    | Conjunctive


type WordCategory
    = Godan
    | Ichidan
    | IAdj
    | NaAdj
    | TaForm
    | TeForm


mostRecentCategory : InflectedWord -> WordCategory
mostRecentCategory word =
    case word.intents of
        [] ->
            word.word.category

        category :: _ ->
            intentToCategory category


intentToString : WordIntent -> String
intentToString intent =
    case intent of
        Negative ->
            "Negative"

        Desire ->
            "Desire"

        Past ->
            "Past"

        Conjunctive ->
            "Conjunctive"


categoryToValidIntents : WordCategory -> List WordIntent
categoryToValidIntents category =
    case category of
        Godan ->
            [ Negative, Desire, Past, Conjunctive ]

        Ichidan ->
            [ Negative, Desire, Past, Conjunctive ]

        IAdj ->
            [ Negative, Past, Conjunctive ]

        NaAdj ->
            [ Negative, Past, Conjunctive ]

        TaForm ->
            []

        TeForm ->
            []


wordToValidIntents : InflectedWord -> List WordIntent
wordToValidIntents word =
    categoryToValidIntents (mostRecentCategory word)


fromBasicWord : BasicWord -> InflectedWord
fromBasicWord word =
    { word = word
    , intents = []
    }


isIntentValid : WordIntent -> WordCategory -> Bool
isIntentValid intent category =
    categoryToValidIntents category
        |> List.member intent


intentToCategory : WordIntent -> WordCategory
intentToCategory intent =
    case intent of
        Negative ->
            IAdj

        Desire ->
            IAdj

        Past ->
            TaForm

        Conjunctive ->
            TeForm


pushIntent : WordIntent -> InflectedWord -> InflectedWord
pushIntent intent word =
    if isIntentValid intent (mostRecentCategory word) then
        { word | intents = intent :: word.intents }

    else
        word


removeIntent : InflectedWord -> InflectedWord
removeIntent word =
    case word.intents of
        [] ->
            { word | intents = [] }

        _ :: intents ->
            { word | intents = intents }


toString : InflectedWord -> String
toString word =
    List.foldr inflect word.word word.intents |> .root
