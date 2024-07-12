module Json.Schema.Form.Options exposing (Options)

import Form.Error exposing (ErrorValue)
import Json.Pointer exposing (Pointer)
import Json.Schema.Form.Theme exposing (Theme)


type alias Options =
    { errors : Pointer -> ErrorValue -> String
    , theme : Theme
    }
