module Examples.LayoutHorizontal exposing (layoutExample1UiSchema)


layoutExample1UiSchema : String
layoutExample1UiSchema =
    """{
  "type": "HorizontalLayout",
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
