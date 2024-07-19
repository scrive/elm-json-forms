module Examples.GenerateUiSchema exposing (generateUiSchemaExampleSchema)


generateUiSchemaExampleSchema : String
generateUiSchemaExampleSchema =
    """{
  "type": "object",
  "properties": {
    "name": {
      "type": "string"
    },
    "vegetarian": {
      "type": "boolean"
    },
    "birthDate": {
      "type": "string"
    },
    "personalData": {
      "type": "object",
      "properties": {
        "age": {
          "type": "integer"
        }
      },
      "additionalProperties": true,
      "required": [
        "age"
      ]
    },
    "postalCode": {
      "type": "string"
    }
  },
  "additionalProperties": true,
  "required": [
    "name",
    "vegetarian",
    "birthDate",
    "personalData",
    "postalCode"
  ]
}"""
