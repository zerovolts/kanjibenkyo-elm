module Route exposing (Route(..), toRoute)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = NotFound
    | Home
    | KanaIndex
    | KanaShow String
    | KanjiIndex
    | WordIndex
    | WordInflector


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map KanaIndex (s "kana")
        , map KanaShow (s "kana" </> string)
        , map KanjiIndex (s "kanji")
        , map WordIndex (s "words")
        , map WordInflector (s "words" </> s "inflector")
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse routeParser url)
