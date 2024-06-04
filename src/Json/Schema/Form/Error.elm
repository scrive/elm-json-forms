module Json.Schema.Form.Error exposing (CustomErrorValue(..), Errors)

{-| Validation errors.

@docs ErrorValue, Errors

-}

import Form.Error


{-| A validation error. See `etaque/elm-form` for more error types.
-}
type CustomErrorValue
    = Invalid
    | InvalidSet
    | ShorterListThan Int
    | LongerListThan Int
    | InvalidCustomFormat String


{-| A function that converts a field path and an `CustomErrorValue` into a user readable string.
-}
type alias Errors =
    String -> Form.Error.ErrorValue CustomErrorValue -> String
