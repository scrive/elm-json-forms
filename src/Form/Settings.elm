module Form.Settings exposing (Settings)

import Form.Error exposing (ErrorValue)
import Form.Theme exposing (Theme)
import Json.Pointer exposing (Pointer)


type alias Settings =
    { errors : Pointer -> ErrorValue -> String
    , theme : Theme
    }
