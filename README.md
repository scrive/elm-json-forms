# JSON Schema Form Generator

[![Elm CI](https://github.com/scrive/elm-json-forms/actions/workflows/elm.yml/badge.svg)](https://github.com/scrive/elm-json-forms/actions/workflows/elm.yml)
[![GitHub Pages](https://github.com/scrive/elm-json-forms/actions/workflows/elm-to-gh-pages.yml/badge.svg)](https://github.com/scrive/elm-json-forms/actions/workflows/elm-to-gh-pages.yml)

Elm Package: https://package.elm-lang.org/packages/scrive/elm-json-forms/latest

Live example: https://scrive.github.io/elm-json-forms/

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

- Linked schemas using `$ref`.
- Arrays
- Control Example
  - Descriptions as tooltips for booleans
- Categorization Example
  - Implement prev/next buttons
  - Implement stepper variant
