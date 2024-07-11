module Json.Schema.Form.Options exposing (Options)

import Dict exposing (Dict)
import Form.Error exposing (ErrorValue)
import Json.Schema.Form.Theme exposing (Theme)


type alias Options =
    { errors : String -> ErrorValue -> String
    , theme : Theme
    }
