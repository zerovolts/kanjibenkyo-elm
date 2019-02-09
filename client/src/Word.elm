module Word exposing
    ( BaseWord
    , DictionaryWord
    , InflectedWord
    , Intent(..)
    , WordCategory(..)
    , allIntents
    , default
    , defaultGodan
    , dictionaryToBaseWord
    , fromBaseWord
    , getKanji
    , intentToCategory
    , intentToString
    , pushIntent
    , removeIntent
    , toKanaString
    , toNegative
    , toPast
    , toString
    , wordToValidIntents
    )

import Dict exposing (Dict)
import Kana exposing (VowelCategory)
import Kanji exposing (Kanji)


type alias BaseWord =
    -- represents a grammatical word base devoid of meaning or other metadata
    -- contains only enough information to form inflections
    -- is not necessarily in dictionary form
    { root : String
    , furigana : String
    , category : WordCategory
    }


type alias DictionaryWord =
    -- represents a word that could be found in a dictionary
    -- should never be inflected
    { root : String
    , furigana : String
    , category : WordCategory
    , meanings : List String
    , isContentWord : Bool
    }


type alias InflectedWord =
    { word : BaseWord
    , intents : List Intent
    }


type
    Intent
    -- intents that facilitate derivational affixes
    = Negative -- 話さない
    | Desire -- 話したい
    | Past -- 話した
      -- TODO: Conjunctive is an inflectional intent
    | Conjunctive -- 話して
    | Potential -- 話せる
    | Progressive -- 話している
    | Volitional -- 話そう
      -- | NaturalConsequence --
      -- | GeneralConditional --
      -- | ContextualConditional --
    | PastConditional -- 話したら・ば
    | Polite -- 話します


type WordCategory
    = Godan
    | Ichidan
    | IAdj
    | NaAdj
    | TaForm
    | TeForm
    | VolitionalForm
    | PastConditionalForm
    | MasuForm


default : DictionaryWord
default =
    { root = "見る"
    , furigana = "みる"
    , category = Ichidan
    , meanings = [ "to see" ]
    , isContentWord = True
    }


defaultGodan : DictionaryWord
defaultGodan =
    { root = "話す"
    , furigana = "はなす"
    , category = Godan
    , meanings = [ "to go" ]
    , isContentWord = True
    }


getKanji : Dict Char Kanji -> BaseWord -> List Kanji
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


allIntents : List Intent
allIntents =
    [ Negative, Desire, Past, Conjunctive, Potential, Progressive, Volitional, PastConditional, Polite ]


mostRecentCategory : InflectedWord -> WordCategory
mostRecentCategory word =
    case word.intents of
        [] ->
            word.word.category

        category :: _ ->
            intentToCategory category


dictionaryToBaseWord word =
    BaseWord word.root word.furigana word.category


intentToString : Intent -> String
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

        Potential ->
            "Potential"

        Progressive ->
            "Progressive"

        Volitional ->
            "Volitional"

        PastConditional ->
            "Past Conditional"

        Polite ->
            "Polite"


categoryToValidIntents : WordCategory -> List Intent
categoryToValidIntents category =
    case category of
        Godan ->
            [ Negative, Desire, Past, Conjunctive, Polite, Progressive, Volitional, Potential ]

        Ichidan ->
            [ Negative, Desire, Past, Conjunctive, Polite, Progressive, Volitional, Potential ]

        IAdj ->
            [ Negative, Past, Conjunctive ]

        NaAdj ->
            [ Negative, Past, Conjunctive ]

        TaForm ->
            [ PastConditional ]

        MasuForm ->
            -- [ Negative, Past, Conjunctive ]
            []

        _ ->
            []


wordToValidIntents : InflectedWord -> List Intent
wordToValidIntents word =
    categoryToValidIntents (mostRecentCategory word)


fromBaseWord : BaseWord -> InflectedWord
fromBaseWord word =
    { word = word
    , intents = []
    }


isIntentValid : Intent -> WordCategory -> Bool
isIntentValid intent category =
    categoryToValidIntents category
        |> List.member intent


intentToCategory : Intent -> WordCategory
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

        Potential ->
            Ichidan

        Progressive ->
            Ichidan

        Volitional ->
            VolitionalForm

        PastConditional ->
            PastConditionalForm

        Polite ->
            MasuForm


pushIntent : Intent -> InflectedWord -> InflectedWord
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


toKanaString : InflectedWord -> String
toKanaString word =
    List.foldr inflect word.word word.intents |> .furigana



-- Inflection Handlers --


inflect : Intent -> BaseWord -> BaseWord
inflect intent word =
    let
        updateRoot =
            case intent of
                Negative ->
                    toNegative

                Past ->
                    toPast

                Conjunctive ->
                    toConjunctive

                Desire ->
                    toDesire

                Potential ->
                    toPotential

                Progressive ->
                    toProgressive

                Volitional ->
                    toVolitional

                PastConditional ->
                    toPastConditional

                Polite ->
                    toPolite
    in
    { word
        | root = updateRoot word.category word.root
        , furigana = updateRoot word.category word.furigana
        , category = intentToCategory intent
    }


toConjunctive : WordCategory -> (String -> String)
toConjunctive category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "て"

        Godan ->
            godanRootToConjunctive

        IAdj ->
            replaceOkurigana 1 "くて"

        _ ->
            identity


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

            else if vowel == 'す' then
                "して"

            else
                -- should never happen
                "-"

        stem =
            String.slice 0 -1 root
    in
    stem ++ newVowel


toPast : WordCategory -> (String -> String)
toPast category =
    case category of
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


toNegative : WordCategory -> (String -> String)
toNegative category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "ない"

        Godan ->
            changeOkuriganaByVowel Kana.A "ない"

        IAdj ->
            replaceOkurigana 1 "くない"

        _ ->
            identity


toDesire : WordCategory -> (String -> String)
toDesire category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "たい"

        Godan ->
            changeOkuriganaByVowel Kana.I "たい"

        _ ->
            identity


toPotential : WordCategory -> (String -> String)
toPotential category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "られる"

        Godan ->
            changeOkuriganaByVowel Kana.E "る"

        _ ->
            identity


toProgressive : WordCategory -> (String -> String)
toProgressive category =
    case category of
        Ichidan ->
            toConjunctive Ichidan >> (\str -> String.append str "いる")

        Godan ->
            toConjunctive Godan >> (\str -> String.append str "いる")

        _ ->
            identity


toVolitional : WordCategory -> (String -> String)
toVolitional category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "よう"

        Godan ->
            changeOkuriganaByVowel Kana.O "う"

        _ ->
            identity


toPastConditional : WordCategory -> (String -> String)
toPastConditional category =
    case category of
        TaForm ->
            \str -> String.append str "ら"

        _ ->
            identity


toPolite : WordCategory -> (String -> String)
toPolite category =
    case category of
        Ichidan ->
            replaceOkurigana 1 "ます"

        Godan ->
            changeOkuriganaByVowel Kana.I "ます"

        _ ->
            identity


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
