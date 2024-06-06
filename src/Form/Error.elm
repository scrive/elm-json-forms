module Form.Error exposing (Error, ErrorValue(..), error)

{-| Validation errors.

@docs Error, ErrorValue, error

-}

import Form.Pointer exposing (Pointer)
import Json.Decode exposing (Value)


type alias Error =
    List ( Pointer, ErrorValue )


type ErrorValue
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

error : ErrorValue -> Error
error e =
    [ ( [], e ) ]
