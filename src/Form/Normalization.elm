module Form.Normalization exposing (normalizeValue)

import Form.Error exposing (ErrorValue(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Result.Extra as Result


{-| If decoder succeeds, return its value. Otherwise, return a default value.
-}
withDefault : Decode.Decoder Value -> Value -> Value
withDefault d v =
    Result.withDefault v <| Decode.decodeValue d v


normalizeValue : Value -> Value
normalizeValue =
    normalizeObject >> normalizeList >> normalizeString


{-| Trim spaces from strings
-}
normalizeString : Value -> Value
normalizeString =
    withDefault (Decode.map (Encode.string << String.trim) Decode.string)


isEmpty : Value -> Bool
isEmpty v =
    Decode.decodeValue Decode.string v == Ok ""


normalizeObject : Value -> Value
normalizeObject value =
    let
        {- Empty values are removed from objects.

           This is important to do, because we need to mark empty fields as missing,
           not invalid.
        -}
        mapKeyValue ( k, v ) =
            if isEmpty v then
                Nothing

            else
                Just ( k, v )
    in
    withDefault (Decode.map (Encode.object << List.filterMap (mapKeyValue << Tuple.mapSecond normalizeValue)) (Decode.keyValuePairs Decode.value)) value


normalizeList : Value -> Value
normalizeList v =
    withDefault (Decode.map (Encode.list normalizeValue) (Decode.list Decode.value)) v
