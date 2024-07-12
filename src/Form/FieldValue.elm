module Form.FieldValue exposing
    ( FieldValue(..)
    , asValue
    , asBool
    , asString
    )

import Json.Encode as Encode exposing (Value)
import String


type FieldValue
    = String String
    | Int Int
    | Number Float
    | Bool Bool
    | Empty


asString : FieldValue -> String
asString fv =
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

        Empty ->
            ""


asValue : FieldValue -> Maybe Value
asValue fv =
    case fv of
        String s ->
            Just <| Encode.string s

        Int i ->
            Just <| Encode.int i

        Number n ->
            Just <| Encode.float n

        Bool b ->
            Just <| Encode.bool b

        Empty ->
            Nothing


asBool : FieldValue -> Maybe Bool
asBool fv =
    case fv of
        Bool b ->
            Just b

        _ ->
            Nothing
