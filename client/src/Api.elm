module Api exposing (decodeKanji, getAllKana, getAllKanji)

import Http
import Json.Decode as D exposing (Decoder)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Msg exposing (Msg(..))


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


getAllKanji : Cmd Msg
getAllKanji =
    Http.send
        AllKanjiData
        (Http.get "http://localhost:8000/kanji" (D.list decodeKanji))


decodeKanji : Decoder Kanji
decodeKanji =
    D.map7 Kanji
        (D.field "character" D.string |> D.andThen strToChar)
        (D.field "onyomi" (D.list D.string))
        (D.field "kunyomi" (D.list D.string))
        (D.field "meanings" (D.list D.string))
        (D.field "jlpt" D.int)
        (D.field "radical" D.string |> D.andThen strToChar)
        (D.field "components" (D.list (D.string |> D.andThen strToChar)))


strToChar : String -> Decoder Char
strToChar str =
    str
        |> String.uncons
        |> Maybe.andThen (\tuple -> Just (Tuple.first tuple))
        |> Maybe.withDefault 'x'
        |> D.succeed
