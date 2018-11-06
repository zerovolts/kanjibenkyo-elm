module Api exposing (decodeKanji, getAllKana, getAllKanji, getAllKanjiIfNeeded)

import Dict exposing (Dict)
import Http
import Json.Decode as D exposing (Decoder)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Msg exposing (Msg(..))
import Route exposing (Route(..))


getAllKana : Cmd Msg
getAllKana =
    Http.send
        AllKanaData
        (Http.get "http://localhost:8000/kana" (D.list decodeKana))


decodeKana : Decoder Kana
decodeKana =
    D.map3 Kana
        (D.field "hiragana" D.string |> D.andThen strToChar)
        (D.field "katakana" D.string |> D.andThen strToChar)
        (D.field "romaji" D.string)


getAllKanjiIfNeeded : List a -> Cmd Msg
getAllKanjiIfNeeded kanji =
    if List.length kanji > 0 then
        Cmd.none

    else
        getAllKanji


getAllKanji : Cmd Msg
getAllKanji =
    Http.send
        AllKanjiData
        (Http.get "http://localhost:8000/kanji" (D.list decodeKanji))


decodeKanji : Decoder Kanji
decodeKanji =
    D.map8 Kanji
        (D.field "character" D.string |> D.andThen strToChar)
        (D.field "strokes" D.int)
        (D.field "onyomi" (D.list D.string))
        (D.field "kunyomi" (D.list D.string))
        (D.field "meanings" (D.list D.string))
        (D.field "grade" D.int)
        (D.field "radical" D.string |> D.andThen strToChar)
        (D.field "components" (D.list (D.string |> D.andThen strToChar)))


strToChar : String -> Decoder Char
strToChar str =
    str
        |> String.uncons
        |> Maybe.andThen (\tuple -> Just (Tuple.first tuple))
        |> Maybe.withDefault 'x'
        |> D.succeed
