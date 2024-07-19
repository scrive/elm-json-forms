module Examples.Rule exposing (..)


ruleExampleSchema : String
ruleExampleSchema =
    """{
  "type": "object",
  "properties": {
    "name": {
      "type": "string"
    },
    "dead": {
      "type": "boolean"
    },
    "kindOfDead": {
      "type": "string",
      "enum": [
        "Zombie",
        "Vampire",
        "Ghoul"
      ]
    },
    "vegetables": {
      "type": "boolean"
    },
    "kindOfVegetables": {
      "type": "string",
      "enum": [
        "All",
        "Some",
        "Only potatoes"
      ]
    }
  }
}"""


ruleExampleUiSchema : String
ruleExampleUiSchema =
    """{
  "type": "VerticalLayout",
  "elements": [
    {
      "type": "Control",
      "label": "Name",
      "scope": "#/properties/name"
    },
    {
      "type": "Group",
      "elements": [
        {
          "type": "Control",
          "label": "Is Dead?",
          "scope": "#/properties/dead"
        },
        {
          "type": "Control",
          "label": "Kind of dead",
          "scope": "#/properties/kindOfDead",
          "rule": {
            "effect": "ENABLE",
            "condition": {
              "scope": "#/properties/dead",
              "schema": {
                "const": true
              }
            }
          }
        }
      ]
    },
    {
      "type": "Group",
      "elements": [
        {
          "type": "Control",
          "label": "Eats vegetables?",
          "scope": "#/properties/vegetables"
        },
        {
          "type": "Control",
          "label": "Kind of vegetables",
          "scope": "#/properties/kindOfVegetables",
          "rule": {
            "effect": "HIDE",
            "condition": {
              "scope": "#/properties/vegetables",
              "schema": {
                "const": false
              }
            }
          }
        }
      ]
    }
  ]
}"""
