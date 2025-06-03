module Validation exposing
    ( Validation
    , error
    , fail
    , isOk
    , mapErrorPointers
    , oneOf
    , succeed
    , unless
    , validateAll
    , voidRight
    , whenJust
    )

-- Inspired by https://hackage.haskell.org/package/validation-selective

import Form.Error as Error exposing (ErrorValue, Errors)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Result
import Result.Extra as Result


type alias Validation output =
    Result Errors output


fail : Errors -> Validation a
fail =
    Err


succeed : a -> Validation a
succeed =
    Ok


isOk : Validation a -> Bool
isOk =
    Result.isOk


whenJust : Maybe b -> (b -> a -> Validation a) -> a -> Validation a
whenJust m f =
    case m of
        Nothing ->
            Ok

        Just b ->
            f b


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
    List.foldl f (Err (error Error.Empty)) validations


{-| Run a list of validations, discard their results
-}
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


mapErrorPointers : (Pointer -> Pointer) -> Validation a -> Validation a
mapErrorPointers f =
    Result.mapError (\l -> List.map (\( p, e ) -> ( f p, e )) l)


andMap : Validation a -> Validation (a -> b) -> Validation b
andMap aValidation partialValidation =
    case ( partialValidation, aValidation ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (List.append (errList partialResult) (errList aResult))


map : (a -> b) -> Validation a -> Validation b
map f v =
    andMap v (Ok f)


voidRight : a -> Validation b -> Validation a
voidRight a =
    map (always a)


errList : Validation a -> Errors
errList res =
    case res of
        Ok _ ->
            []

        Err e ->
            e


unless : Bool -> ErrorValue -> a -> Validation a
unless p e a =
    if p then
        Ok a

    else
        Err <| error e


error : ErrorValue -> Errors
error e =
    [ ( [], e ) ]
