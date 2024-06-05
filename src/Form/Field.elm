module Form.Field exposing
    ( FieldValue(..)
    , valueAsBool, valueAsString
    )

{-| Read and write field values.


# Constructors

@docs Field, FieldValue, value, string, bool, group, list


# Value readers

@docs asString, asBool

-}

import String


{-| Form field. Can either be a group of named fields, or a final field.
-}
type FieldValue
    = String String
    | Int Int
    | Number Float
    | Bool Bool



-- | EmptyField


valueAsString : FieldValue -> Maybe String
valueAsString fv =
    case fv of
        String s ->
            Just s

        Int i ->
            Just <| String.fromInt i

        Number n ->
            Just <| String.fromFloat n

        Bool _ ->
            Nothing


valueAsBool : FieldValue -> Maybe Bool
valueAsBool fv =
    case fv of
        Bool b ->
            Just b

        _ ->
            Nothing
