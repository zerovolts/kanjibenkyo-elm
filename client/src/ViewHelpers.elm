module ViewHelpers exposing (defaultSpacing, unit)


unit : Int -> Int
unit x =
    x * 16


defaultSpacing : Int
defaultSpacing =
    unit 1
