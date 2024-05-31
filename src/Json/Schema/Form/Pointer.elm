module Json.Schema.Form.Pointer exposing
    ( Pointer
    , decode
    , encode
    , fromString
    , toString
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
    Decode.string
        |> Decode.andThen
            (\s ->
                case fromString s of
                    Ok x ->
                        Decode.succeed x

                    Err x ->
                        Decode.fail x
            )


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


encode : Pointer -> Encode.Value
encode p =
    Encode.string (toString p)


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
