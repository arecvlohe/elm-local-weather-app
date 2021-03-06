module Types exposing (..)


type alias Position =
    { latitude : Float
    , longitude : Float
    }


type alias Error =
    { status : Int
    , message : String
    }


type alias CurrentTemp =
    Float


type alias Fetch =
    Bool
