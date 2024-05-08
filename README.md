# JSON Schema Form Generator

[![Elm CI](https://github.com/scrive/json-schema-form/workflows/Elm%20CI/badge.svg)](https://github.com/scrive/json-schema-form/actions)

Generate validating forms from JSON schemas.

    elm package install scrive/json-schema-form

## Features

- Can handle almost all JSON Schema features (draft-04 and draft-06).
- Generates all common types of input fields (`text`, `select`, etc.) with optional labels and descriptions.
- Error messages can easily be customized as needed.
- Supports custom string formats using validation functions (similar to Json decoders).
- Comes with default Bootstrap and Tailwind CSS themes in the `Theme` object, that can also be customised.

## Warnings

1. The way form fields are generated and presented is very opinionated and thus not always suitable for general case usage. This library is intended to be used for cases where you have control over how the schema is structured.
2. There is currently no support for linked schemas using `$ref`.

## Example usage

See the [example project](https://github.com/scrive/json-schema-form/tree/master/example) for examples of all the supported field types.

```elm
module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (onSubmit)
import Json.Schema
import Json.Schema.Form exposing (Msg, State)
import Json.Schema.Form.Theme as Theme


main : Program () State Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


schema =
  """
  {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "type": "string",
        "title": "Name"
      }
    }
  }
  """


init : State
init =
    case Json.Schema.fromString schema of
        Ok schema_ ->
            Json.Schema.Form.init
                { errors = \path error -> "Invalid field: " ++ path
                , formats = Dict.empty
                , theme = Theme.default
                }
                schema_

        Err error ->
            Debug.todo error


update : Msg -> State -> State
update msg state =
    Json.Schema.Form.update msg state


view : State -> Html Msg
view state =
    form [ onSubmit Json.Schema.Form.submit ]
        [ Json.Schema.Form.view state
        , button [] [ text "Submit" ]
        ]
```
