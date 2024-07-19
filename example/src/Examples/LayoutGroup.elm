module Examples.LayoutGroup exposing (..)


layoutExample3UiSchema : String
layoutExample3UiSchema =
    """{
  "type": "Group",
  "label": "My Group",
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
