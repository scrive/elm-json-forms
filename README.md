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

## Unimplemented features

- [x] Fix multiple forms on the same page sharing IDs
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

1. The way form fields are generated and presented is very opinionated and thus not always suitable for general case usage. This library is intended to be used for cases where you have control over how the schema is structured.
2. There is currently no support for linked schemas using `$ref`.

## Example usage

See the [example project](https://github.com/scrive/json-schema-form/tree/master/example) for examples of all the supported field types.
