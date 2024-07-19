module Json.Pointer exposing
    ( Pointer
    , decode
    , fromString
    , pointedValue
    , toString
    )

{-| This module implements JSON Pointer as per [RFC 6901](https://tools.ietf.org/html/rfc6901).

@docs Pointer, decode, fromString, toString, pointedValue
-}

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import String

{-| Pointer path, represented as `List String`.


Pointers have a standardized text format. For example:

```
"#/customers/0/name"
```
-}
type alias Pointer =
    List String


{-| Decode a pointer
-}
decode : Decode.Decoder Pointer
decode =
    Decode.string
        |> Decode.andThen
            (\s ->
                case fromString s of
                    Ok x ->
                        Decode.succeed x

                    Err x ->
                        Decode.fail x
            )


{-| Construct a pointer from a `String`.
-}
fromString : String -> Result String Pointer
fromString string =
    case splitAndUnescape string of
        "#" :: pointer ->
            Ok pointer

        _ ->
            Err "A JSON Pointer must start with #"


splitAndUnescape : String -> List String
splitAndUnescape string =
    string |> String.split "/" |> List.map unescape


unescape : String -> String
unescape string =
    string
        |> String.split "~1"
        |> String.join "/"
        |> String.split "~0"
        |> String.join "~"


{-| Convert a pointer to a `String`.
-}
toString : Pointer -> String
toString =
    List.append [ "#" ] >> List.map escape >> String.join "/"


escape : String -> String
escape string =
    string
        |> String.split "~"
        |> String.join "~0"
        |> String.split "/"
        |> String.join "~1"


{-| Get a sub-value which is pointed at by a pointer.

If the pointer does not point to an existing value, `Nothing` is returned.
-}
pointedValue : Pointer -> Encode.Value -> Maybe Encode.Value
pointedValue pointer value =
    case pointer of
        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok dict ->
                    Maybe.andThen (pointedValue ps) <| Dict.get key dict

                Err _ ->
                    Nothing

        [] ->
            Just value

        _ ->
            Nothing
