module Json.Schema.Form.Encode exposing (encode)

{-| Encode form values as JSON.

@docs encode

-}

import Json.Encode as Encode
import Json.Schema.Form.Value exposing (Value(..))


{-| Encode a form value (the output of a valid form) as JSON.
-}
encode : Value -> Encode.Value
encode value =
    case value of
        IntValue intValue ->
            Encode.int intValue

        FloatValue floatValue ->
            Encode.float floatValue

        StringValue stringValue ->
            Encode.string stringValue

        BoolValue boolValue ->
            Encode.bool boolValue

        ListValue valueList ->
            Encode.list encode valueList

        ObjectValue objectValue ->
            let
                item : ( String, Value ) -> Maybe ( String, Encode.Value )
                item ( name, val ) =
                    if val == EmptyValue then
                        Nothing

                    else
                        Just ( name, encode val )
            in
            Encode.object (List.filterMap item objectValue)

        NullValue ->
            Encode.null

        EmptyValue ->
            Encode.object []

        JsonValue jsonValue ->
            jsonValue
