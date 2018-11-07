module User exposing (User, default)


type alias User =
    { name : String
    , email : String
    , experience : Int
    }


default : User
default =
    { name = "Guest"
    , email = "guest@kanjibenkyo.com"
    , experience = 0
    }
