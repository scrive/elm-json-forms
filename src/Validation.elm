module Validation exposing
    ( Validation
    , andMap
    , bool
    , fail
    , float
    , format
    , isOk
    , map
    , mapError
    , mapErrorPointers
    , maxLength
    , maybe
    , minLength
    , oneOf
    , succeed
    , unless
    , validateAll
    , whenJust
    )

-- Inspired by https://hackage.haskell.org/package/validation-selective

import Form.Error as Error exposing (ErrorValue, Errors)
import Json.Decode as Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Regex exposing (Regex)
import Result
import String


type alias Validation output =
    Result Errors output


isOk : Validation a -> Bool
isOk v =
    case v of
        Ok _ ->
            True

        Err _ ->
            False


map : (a -> b) -> Validation a -> Validation b
map =
    Result.map


andMap : Validation a -> Validation (a -> b) -> Validation b
andMap aValidation partialValidation =
    case ( partialValidation, aValidation ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (List.append (errList partialResult) (errList aResult))


mapError : (Errors -> Errors) -> Validation a -> Validation a
mapError =
    Result.mapError


mapErrorPointers : (Pointer -> Pointer) -> Validation a -> Validation a
mapErrorPointers f =
    mapError (\l -> List.map (\( p, e ) -> ( f p, e )) l)


errList : Validation a -> Errors
errList res =
    case res of
        Ok _ ->
            []

        Err e ->
            e


float : Value -> Validation Float
float =
    Result.mapError (\_ -> Error.error Error.InvalidFloat) << Decode.decodeValue Decode.float


bool : Value -> Validation Bool
bool =
    Result.mapError (\_ -> Error.error Error.InvalidBool) << Decode.decodeValue Decode.bool


{-| Transform validation result to `Maybe`, using `Result.toMaybe`.
-}
maybe : Validation a -> Validation (Maybe a)
maybe =
    Ok << Result.toMaybe


{-| Min length for String.
-}
minLength : Int -> String -> Validation String
minLength min s =
    if String.length s >= min then
        Ok s

    else
        Err (Error.error (Error.ShorterStringThan min))


{-| Max length for String.
-}
maxLength : Int -> String -> Validation String
maxLength max s =
    if String.length s <= max then
        Ok s

    else
        Err (Error.error (Error.LongerStringThan max))


{-| Validates format of the string.
-}
format : Regex -> String -> Validation String
format regex s =
    if Regex.contains regex s then
        Ok s

    else
        Err (Error.error Error.InvalidFormat)


{-| A validation that always fails. Useful for contextual validation.
-}
fail : Errors -> Validation a
fail error =
    Err error


{-| A validation that always succeeds. Useful for contextual validation.
-}
succeed : a -> Validation a
succeed a =
    Ok a


{-| First successful validation wins, from left to right.
-}
oneOf : List (Value -> Validation a) -> Value -> Validation a
oneOf validations v =
    let
        f : (Value -> Validation a) -> Validation a -> Validation a
        f a b =
            case a v of
                Ok _ ->
                    a v

                Err _ ->
                    b
    in
    List.foldl f (Err (Error.error Error.Empty)) validations


validateAll : List (a -> Validation b) -> a -> Validation a
validateAll l a =
    let
        validations =
            List.map (\v -> v a) l
    in
    if List.all isOk validations then
        Ok a

    else
        Err <| List.concatMap errList validations


unless : Bool -> ErrorValue -> a -> Validation a
unless p e a =
    if p then
        Ok a

    else
        Err [ ( [], e ) ]


whenJust : Maybe b -> (b -> a -> Validation a) -> a -> Validation a
whenJust m f =
    case m of
        Nothing ->
            Ok

        Just b ->
            f b
