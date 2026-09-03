module Json.Pointer exposing (Pointer, decode, fromString, toString, pointedValue)

{-| This module implements JSON Pointer as per [RFC 6901](https://tools.ietf.org/html/rfc6901).

@docs Pointer, decode, fromString, toString, pointedValue

-}

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import String


{-| Pointer path, represented as `List String`.
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


{-| Construct a pointer from the standardized text format. Either of the following formats is valid:

    /foo/0/bar
    #/foo/0/bar

URL decoding is not performed by this function.
-}
fromString : String -> Result String Pointer
fromString string =
    case splitAndUnescape string of
        "#" :: pointer ->
            Ok pointer

        "" :: pointer ->
            Ok pointer

        _ ->
            Err "Pointer must start with #/ or /"


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


{-| Convert a pointer to the standardized text format:

    #/foo/0/bar

URL encoding is not performed by this function.
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
