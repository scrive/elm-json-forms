module Examples.LayoutVertical exposing (layoutExample2UiSchema)


layoutExample2UiSchema : String
layoutExample2UiSchema =
    """{
  "type": "VerticalLayout",
  "elements": [
    {
      "type": "Control",
      "label": "Name",
      "scope": "#/properties/name"
    },
    {
      "type": "Control",
      "label": "Birth Date",
      "scope": "#/properties/birthDate"
    }
  ]
}"""
