module Form.Options exposing (Options)

import Form.Error exposing (ErrorValue)
import Form.View.Theme exposing (Theme)
import Json.Pointer exposing (Pointer)


type alias Options =
    { errors : Pointer -> ErrorValue -> String
    , theme : Theme
    }
