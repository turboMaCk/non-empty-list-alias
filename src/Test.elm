module Test exposing (..)


type Id a
    = Id String


type alias WithId r =
    { r | id : Id r }


type alias User =
    WithId
        { name : String
        , age : Int
        }


user : User
user =
    { id = Id "1"
    , name = "Jane Doe"
    , age = 54
    }
