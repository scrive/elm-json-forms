module Json.Schema.Form.Options exposing (Options)

import Dict exposing (Dict)
import Form.Error exposing (ErrorValue)
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Theme exposing (Theme)


type alias Options =
    { errors : String -> ErrorValue -> String
    , formats : Dict String Format
    , theme : Theme
    }
