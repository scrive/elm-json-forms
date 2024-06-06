module Form.Validate exposing
    ( Validation
    , andMap
    , bool
    , customError
    , fail
    , float
    , format
    , map
    , mapError
    , mapErrorPointers
    , maxLength
    , maybe
    , minLength
    , nonEmpty
    , oneOf
    , succeed
    , unless
    , validateAll
    , whenJust
    , withCustomError
    )

import Dict exposing (Dict)
import Form.Error as Error exposing (Error, ErrorValue)
import Form.Field as Field exposing (FieldValue)
import Form.Pointer as Pointer exposing (Pointer)
import Json.Decode as Decode exposing (Value)
import Regex exposing (Regex)
import Result
import String


type alias Validation customError output =
    Result (Error customError) output


isOk : Validation e a -> Bool
isOk v =
    case v of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Validation e a -> Bool
isErr =
    not << isOk


map : (a -> b) -> Validation e a -> Validation e b
map =
    Result.map


andMap : Validation e a -> Validation e (a -> b) -> Validation e b
andMap aValidation partialValidation =
    case ( partialValidation, aValidation ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (List.append (errList partialResult) (errList aResult))


mapError : (Error e1 -> Error e2) -> Validation e1 a -> Validation e2 a
mapError =
    Result.mapError


mapErrorPointers : (Pointer -> Pointer) -> Validation e a -> Validation e a
mapErrorPointers f =
    mapError (\l -> List.map (\( p, e ) -> ( f p, e )) l)


withCustomError : customErr -> Validation e a -> Validation customErr a
withCustomError e =
    mapError (\_ -> customError e)


customError : e -> Error e
customError =
    Error.error << Error.CustomError


errList : Validation e a -> Error e
errList res =
    case res of
        Ok _ ->
            []

        Err e ->
            e


float : Value -> Validation e Float
float =
    Result.mapError (\_ -> Error.error Error.InvalidFloat) << Decode.decodeValue Decode.float


bool : Value -> Validation e Bool
bool =
    Result.mapError (\_ -> Error.error Error.InvalidBool) << Decode.decodeValue Decode.bool


{-| Transform validation result to `Maybe`, using `Result.toMaybe`.
-}
maybe : Validation e a -> Validation e (Maybe a)
maybe =
    Ok << Result.toMaybe


{-| Fails if `String.isEmpty`.
-}
nonEmpty : String -> String -> Validation e String
nonEmpty path s =
    if String.isEmpty s then
        Err (Error.error Error.Empty)

    else
        Ok s


{-| Min length for String.
-}
minLength : Int -> String -> Validation e String
minLength min s =
    if String.length s >= min then
        Ok s

    else
        Err (Error.error (Error.ShorterStringThan min))


{-| Max length for String.
-}
maxLength : Int -> String -> Validation e String
maxLength max s =
    if String.length s <= max then
        Ok s

    else
        Err (Error.error (Error.LongerStringThan max))


{-| Validates format of the string.
-}
format : Regex -> String -> Validation e String
format regex s =
    if Regex.contains regex s then
        Ok s

    else
        Err (Error.error Error.InvalidFormat)


{-| Stolen to elm-validate.
-}
validEmailPattern : Regex
validEmailPattern =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


{-| A validation that always fails. Useful for contextual validation.
-}
fail : Error e -> Validation e a
fail error =
    Err error


{-| A validation that always succeeds. Useful for contextual validation.
-}
succeed : a -> Validation e a
succeed a =
    Ok a


{-| First successful validation wins, from left to right.
-}
oneOf : List (Validation e a) -> Validation e a
oneOf validations =
    let
        walkResults result combined =
            case ( combined, result ) of
                ( Ok _, _ ) ->
                    combined

                _ ->
                    result
    in
    List.foldl walkResults (Err (Error.error Error.Empty)) validations


validateAll : List (a -> Validation e b) -> a -> Validation e a
validateAll l a =
    let
        validations =
            List.map (\v -> v a) l
    in
    if List.all isOk validations then
        Ok a

    else
        Err <| List.concat <| List.map errList validations


unless : Bool -> ErrorValue e -> a -> Validation e a
unless p e a =
    if p then
        Ok a

    else
        Err [ ( [], e ) ]


whenJust : Maybe b -> (b -> a -> Validation e a) -> a -> Validation e a
whenJust m f =
    case m of
        Nothing ->
            Ok

        Just b ->
            f b
