module Examples.Layout exposing (..)



layoutExampleSchema : String
layoutExampleSchema =
    """
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "minLength": 3,
      "description": "Please enter your name"
    },
    "birthDate": {
      "type": "string",
      "format": "date"
    }
  }
}
"""
