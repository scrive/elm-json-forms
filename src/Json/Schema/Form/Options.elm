module Json.Schema.Form.Options exposing (Options)

import Dict exposing (Dict)
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Theme exposing (Theme)
import Form.Error exposing (ErrorValue)


type alias Options =
    { errors : String -> ErrorValue -> String
    , formats : Dict String Format
    , theme : Theme
    }
