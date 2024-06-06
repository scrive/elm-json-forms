module Form.Error exposing (Error, ErrorValue(..), error)

{-| Validation errors.

@docs Error, ErrorValue, error

-}

import Form.Pointer as Pointer exposing (Pointer)
import Json.Decode exposing (Value)


type alias Error customError =
    List ( Pointer, ErrorValue customError )


{-| A validation error. See `Form.Validate.customError` for `CustomError` building.
-}
type ErrorValue e
    = Empty
    | InvalidString
    | InvalidEmail
    | InvalidFormat
    | InvalidInt
    | InvalidFloat
    | InvalidBool
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
    | CustomError e


{-| Build a singleton error value
-}
error : ErrorValue a -> Error a
error e =
    [ ( [], e ) ]
