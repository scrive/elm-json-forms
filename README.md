# JSON Schema Form Generator

[![Elm CI](https://github.com/scrive/json-schema-form/workflows/Elm%20CI/badge.svg)](https://github.com/scrive/json-schema-form/actions)

Generate validating forms from JSON schemas.

    elm package install potocpav/elm-json-forms

## Features

- Can handle most of the UI Schema specification
- Can handle almost all JSON Schema features (draft-04 and draft-06).
- Generates all common types of input fields (`text`, `select`, etc.) with optional labels and descriptions.
- Error messages can easily be customized as needed.
- Supports custom string formats using validation functions (similar to Json decoders).
- Comes with default Tailwind CSS theme in the `Theme` object that can be customised.

## Fresh and Unimplemented Features

- [x] Fix multiple forms on the same page sharing IDs
- [x] Validate e-mails
- [x] Layout
  - [x] Remove unnecessary nested formRow items for horizontal layout inside vertical layout
- [ ] Control Example
  - [ ] Descriptions as tooltips for Booleans
  - [ ] Asterisks for required fields
  - [ ] Toggle buttons for booleans
  - [ ] Trim text
  - [ ] Restrict text
  - [ ] Hide required asterisk
- [ ] Categorization Example
  - [x] Fix multiple categorizations in one schema (in vertical layout)
  - [x] Rules to show and hide categories
  - [ ] Implement prev/next buttons
  - [ ] Implement stepper variant
- [x] Rule example
  - [x] Groups
  - [x] Group labels
  - [x] Rule to enable/disable
  - [x] Rule to show/hide
- [x] Layout example
- [x] Generate UI Schema example
  - [x] Fix groups

## Warnings

1. There is currently no support for linked schemas using `$ref`.

## Example usage

See the [example project](https://github.com/potocpav/elm-json-form/tree/master/example) for examples
