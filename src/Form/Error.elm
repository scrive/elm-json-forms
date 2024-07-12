module Form.Error exposing
    ( ErrorValue(..)
    , Errors
    , error
    , getErrors
    )

import Json.Decode exposing (Value)
import Json.Pointer as Pointer exposing (Pointer)


type alias Errors =
    List ( Pointer, ErrorValue )


type ErrorValue
    = Empty
    | InvalidString
    | InvalidEmail
    | InvalidFormat
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


error : ErrorValue -> Errors
error e =
    [ ( [], e ) ]


getErrors : Errors -> List ( String, ErrorValue )
getErrors errors =
    List.map (\( p, e ) -> ( Pointer.toString p, e )) errors
