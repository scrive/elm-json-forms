# JSON Schema Form Generator

[![Elm CI](https://github.com/scrive/elm-json-forms/workflows/Elm%20CI/badge.svg)](https://github.com/scrive/elm-json-forms/actions)

Generate validating forms from JSON schemas.

    elm package install scrive/elm-json-forms

## Features

- Can handle most of the UI Schema specification
- Can handle almost all JSON Schema features (draft-04 and draft-06).
- Generates all common types of input fields (`text`, `select`, etc.) with optional labels and descriptions.
- Error messages can easily be customized as needed.
- Supports custom string formats using validation functions (similar to Json decoders).
- Comes with default Tailwind CSS theme in the `Theme` object that can be customised.

## Unimplemented Features

- linked schemas using `$ref`.
- Arrays
- Control Example
  - Descriptions as tooltips for booleans
  - Asterisks for required fields
  - Toggle buttons for booleans
  - Trim text
  - Restrict text
  - Hide required asterisk
- Categorization Example
  - Implement prev/next buttons
  - Implement stepper variant

## Example usage

See the [example project](example) for examples
