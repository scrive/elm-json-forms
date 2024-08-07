module Form.Error exposing (Errors, ErrorValue(..), TextFormat(..))

{-| Form field error handling

@docs Errors, ErrorValue, TextFormat

-}

import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)


{-| List of all errors in a form. One field may generate multiple entries.
-}
type alias Errors =
    List ( Pointer, ErrorValue )


{-| A single error value for a single field
-}
type ErrorValue
    = Empty
    | InvalidString
    | InvalidFormat TextFormat
    | InvalidInt
    | InvalidFloat
    | InvalidBool
    | InvalidNull
    | NotConst Value
    | NotMultipleOfInt Int
    | LessIntThan Int
    | LessEqualIntThan Int
    | GreaterIntThan Int
    | GreaterEqualIntThan Int
    | NotMultipleOfFloat Float
    | LessFloatThan Float
    | LessEqualFloatThan Float
    | GreaterFloatThan Float
    | GreaterEqualFloatThan Float
    | ShorterStringThan Int
    | LongerStringThan Int
    | NotIncludedIn (List Value)
    | Unimplemented String


{-| Text format
-}
type TextFormat
    = Regex
    | Time
    | Date
    | DateTime
    | Email
    | Phone
