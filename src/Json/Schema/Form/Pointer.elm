module Json.Schema.Form.Pointer exposing
    ( Pointer
    , decode
    , encode
    )

-- This module implements JSON Pointer as per [RFC 6901](https://tools.ietf.org/html/rfc6901).

import Array
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Regex
import String


type alias Pointer =
    List String


decode : Decode.Decoder Pointer
decode =
    Decode.string |> Decode.andThen pointerFromString


pointerFromString : String -> Decode.Decoder Pointer
pointerFromString string =
    case splitAndUnescape string of
        [ "" ] ->
            Decode.succeed []

        "" :: pointer ->
            Decode.succeed pointer

        _ ->
            Decode.fail "A JSON Pointer must start with / or be empty"


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


encode : Pointer -> Encode.Value
encode p =
    Encode.string (pointerToString p)


pointerToString : Pointer -> String
pointerToString pointer =
    case pointer of
        [] ->
            ""

        _ ->
            pointer
                |> List.map escape
                |> String.join "/"
                |> String.append "/"


escape : String -> String
escape string =
    string
        |> String.split "~"
        |> String.join "~0"
        |> String.split "/"
        |> String.join "~1"
