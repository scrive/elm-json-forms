module Form.Field exposing
    ( FieldValue(..)
    , valueAsBool, valueAsString
    , asValue
    )

{-| Read and write field values.


# Constructors

@docs Field, FieldValue, value, string, bool, group, list


# Value readers

@docs asString, asBool

-}

import String
import Json.Encode as Encode exposing (Value)


{-| Form field. Can either be a group of named fields, or a final field.
-}
type FieldValue
    = String String
    | Int Int
    | Number Float
    | Bool Bool
    | Empty



-- | EmptyField


valueAsString : FieldValue -> String
valueAsString fv =
    case fv of
        String s ->
            s

        Int i ->
            String.fromInt i

        Number n ->
            String.fromFloat n

        Bool True ->
            "True"

        Bool False ->
            "False"

        Empty -> ""

asValue : FieldValue -> Maybe Value
asValue fv = case fv of
    String s ->
        Just <| Encode.string s

    Int i ->
       Just <| Encode.int i

    Number n ->
        Just <| Encode.float n

    Bool b ->
        Just <| Encode.bool b

    Empty -> Nothing


valueAsBool : FieldValue -> Maybe Bool
valueAsBool fv =
    case fv of
        Bool b ->
            Just b

        _ ->
            Nothing
