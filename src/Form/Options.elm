module Form.Options exposing (Options)

import Form.Error exposing (ErrorValue)
import Json.Pointer exposing (Pointer)
import Form.View.Theme exposing (Theme)

type alias Options =
    { errors : Pointer -> ErrorValue -> String
    , theme : Theme
    }
