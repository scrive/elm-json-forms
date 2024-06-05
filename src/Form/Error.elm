module Form.Error exposing (Error, ErrorValue(..), error)

{-| Validation errors.

@docs Error, ErrorValue, error

-}

import Form.Pointer as Pointer exposing (Pointer)


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
    | SmallerIntThan Int
    | GreaterIntThan Int
    | SmallerFloatThan Float
    | GreaterFloatThan Float
    | ShorterStringThan Int
    | LongerStringThan Int
    | NotIncludedIn
    | Unimplemented String
    | CustomError e


{-| Build a singleton error value
-}
error : ErrorValue a -> Error a
error e =
    [ ( [], e ) ]
