module Main exposing (main)

import Browser
import Form exposing (Form, Msg)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Theme as Theme
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onSubmit)
import Json.Encode as Encode
import Json.Schema
import Json.Schema.Definitions exposing (Schema)
import List.Extra as List
import UiSchema exposing (UiSchema)


type alias MainState =
    { forms : List Form
    , titles : List String
    }


type alias FormSpec =
    { title : String
    , schema : Schema
    , uiSchema : Maybe UiSchema
    }


type alias MainMsg =
    { formId : Int
    , msg : Msg
    }


formSpec : String -> String -> Maybe String -> Result String FormSpec
formSpec title schemaText uiSchemaText =
    let
        schema =
            Json.Schema.fromString schemaText

        uiSchema =
            Maybe.map UiSchema.fromString uiSchemaText
    in
    case ( schema, uiSchema ) of
        ( Ok s, Nothing ) ->
            Ok
                { title = title
                , schema = s
                , uiSchema = Nothing
                }

        ( Ok s, Just (Ok us) ) ->
            Ok
                { title = title
                , schema = s
                , uiSchema = Just us
                }

        ( Err e, _ ) ->
            Err e

        ( _, Just (Err e) ) ->
            Err e


main : Program () MainState MainMsg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : MainState
init =
    let
        formSpecs =
            [ formSpec "Basic Example" basicExampleSchema (Just basicExampleUiSchema)
            , formSpec "Control Example 1" controlExample1Schema (Just controlExample1UiSchema)
            , formSpec "Control Example 2" controlExample2Schema (Just controlExample2UiSchema)
            , formSpec "Categorization Example 1" categorizationExample1Schema (Just categorizationExample1UiSchema)
            , formSpec "Rule Example" ruleExampleSchema (Just ruleExampleUiSchema)
            , formSpec "Layout Example: Horizontal layout" layoutExampleSchema (Just layoutExample1UiSchema)
            , formSpec "Layout Example: Vertical layout" layoutExampleSchema (Just layoutExample2UiSchema)
            , formSpec "Layout Example: Group" layoutExampleSchema (Just layoutExample3UiSchema)
            , formSpec "Layout Example: Nested layouts" layoutExampleSchema (Just layoutExample4UiSchema)
            , formSpec "Generate UI Schema Example" generateUiSchemaExampleSchema Nothing
            , formSpec "Testing Schema" testingSchema Nothing
            ]
    in
    { forms =
        List.map
            (\formSpecRes ->
                case formSpecRes of
                    Ok fs ->
                        Form.init
                            fs.title
                            { errors = always errorString
                            , theme = Theme.tailwind
                            }
                            fs.schema
                            fs.uiSchema

                    Err error ->
                        Debug.todo error
            )
            formSpecs
    , titles = List.map (\x -> Result.withDefault "(no title)" <| Result.map .title x) formSpecs
    }


update : MainMsg -> MainState -> MainState
update msg state =
    { state
        | forms =
            List.indexedMap
                (\i f ->
                    if msg.formId == i then
                        Form.update msg.msg f

                    else
                        f
                )
                state.forms
    }


view : MainState -> Html MainMsg
view state =
    div [] <| List.indexedMap (\i ( title, form ) -> Html.map (\m -> { formId = i, msg = m }) (viewForm title form)) (List.zip state.titles state.forms)


viewForm : String -> Form -> Html Msg
viewForm title form =
    let
        anyErrors =
            not <| List.isEmpty <| Error.getErrors form.state.errors
    in
    div []
        [ h1 [ Attrs.class "font-bold text-2xl" ] [ text title ]
        , Html.form []
            [ Form.view form
            , let
                json =
                    Encode.encode 4 form.state.value
              in
              pre
                [ if anyErrors then
                    class "text-red-500"

                  else
                    class ""
                ]
                [ text json ]
            ]
        , hr [ class "my-5" ] []
        ]


errorString : ErrorValue -> String
errorString error =
    case error of
        Empty ->
            "is a required property"

        NotConst v ->
            case Encode.encode 0 v of
                "true" ->
                    "must be checked"

                "false" ->
                    "must be unchecked"

                s ->
                    "must be equal to " ++ s

        InvalidString ->
            "not a valid string"

        InvalidEmail ->
            "not a valid email address"

        InvalidFormat ->
            "not the correct format"

        InvalidInt ->
            "not a valid integer"

        InvalidFloat ->
            "not a valid number"

        InvalidBool ->
            "not a valid option"

        InvalidNull ->
            "not a null"

        LessIntThan n ->
            "can not be smaller than " ++ String.fromInt n

        LessEqualIntThan n ->
            "can not be smaller or equal than " ++ String.fromInt n

        GreaterIntThan n ->
            "can not be greater than " ++ String.fromInt n

        GreaterEqualIntThan n ->
            "can not be greater or equal than " ++ String.fromInt n

        LessFloatThan n ->
            "can not be smaller than " ++ String.fromFloat n

        LessEqualFloatThan n ->
            "can not be smaller or equal than " ++ String.fromFloat n

        GreaterFloatThan n ->
            "can not be greater than " ++ String.fromFloat n

        GreaterEqualFloatThan n ->
            "can not be greater or equal than " ++ String.fromFloat n

        ShorterStringThan n ->
            "must NOT have fewer than " ++ String.fromInt n ++ " characters"

        LongerStringThan n ->
            "must NOT have more than " ++ String.fromInt n ++ " characters"

        NotMultipleOfInt n ->
            "must be a multiple of " ++ String.fromInt n ++ "."

        NotMultipleOfFloat n ->
            "must be a multiple of " ++ String.fromFloat n ++ "."

        NotIncludedIn _ ->
            "is not a valid selection from the list."

        Unimplemented s ->
            "unimplemented: " ++ s


testingSchema : String
testingSchema =
    """
{
  "type": "object",
  "required": [
    "age"
  ],
  "properties": {
    "firstName": {
      "type": "string",
      "minLength": 2,
      "maxLength": 20,
      "const": "abcd"
    },
    "lastName": {
      "type": "string",
      "minLength": 5,
      "maxLength": 15
    },
    "email": {
      "type": "string",
      "format": "email",
      "minLength": 5,
      "maxLength": 15
    },
    "age": {
      "type": "integer",
      "minimum": 18,
      "maximum": 100,
      "multipleOf": 2
    },
    "gender": {
      "type": "string",
      "enum": [
        "Male",
        "Female",
        "Undisclosed"
      ]
    },
    "height": {
      "type": "number",
      "exclusiveMinimum": 0,
      "multipleOf": 1.1
    },
    "dateOfBirth": {
      "type": "string",
      "format": "date"
    },
    "rating": {
      "type": "integer",
      "enum": [1,2,3,4,5]
    },
    "committer": {
      "type": "boolean"
    },
    "address": {
      "type": "object",
      "properties": {
        "street": {
          "type": "string"
        },
        "streetnumber": {
          "type": "string"
        },
        "postalCode": {
          "type": "string"
        },
        "city": {
          "type": "string"
        }
      }
    }
  }
}
    """


basicExampleSchema : String
basicExampleSchema =
    """
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "minLength": 3,
      "description": "Please enter your name"
    },
    "vegetarian": {
      "type": "boolean"
    },
    "birthDate": {
      "type": "string",
      "format": "date"
    },
    "nationality": {
      "type": "string",
      "enum": [
        "DE",
        "IT",
        "JP",
        "US",
        "RU",
        "Other"
      ]
    },
    "personalData": {
      "type": "object",
      "properties": {
        "age": {
          "type": "integer",
          "description": "Please enter your age."
        },
        "height": {
          "type": "number"
        },
        "drivingSkill": {
          "type": "number",
          "maximum": 10,
          "minimum": 1,
          "default": 7
        }
      },
      "required": [
        "age",
        "height"
      ]
    },
    "occupation": {
      "type": "string"
    },
    "postalCode": {
      "type": "string",
      "maxLength": 5
    }
  },
  "required": [
    "occupation",
    "nationality"
  ]
}
    """


basicExampleUiSchema : String
basicExampleUiSchema =
    """
{
  "type": "VerticalLayout",
  "elements": [
    {
      "type": "HorizontalLayout",
      "elements": [
        {
          "type": "Control",
          "scope": "#/properties/name"
        },
        {
          "type": "Control",
          "scope": "#/properties/personalData/properties/age"
        },
        {
          "type": "Control",
          "scope": "#/properties/birthDate"
        }
      ]
    },
    {
      "type": "Label",
      "text": "Additional Information"
    },
    {
      "type": "HorizontalLayout",
      "elements": [
        {
          "type": "Control",
          "scope": "#/properties/personalData/properties/height"
        },
        {
          "type": "Control",
          "scope": "#/properties/nationality"
        },
        {
          "type": "Control",
          "scope": "#/properties/occupation",
          "suggestion": [
            "Accountant",
            "Engineer",
            "Freelancer",
            "Journalism",
            "Physician",
            "Student",
            "Teacher",
            "Other"
          ]
        }
      ]
    }
  ]
}
"""


controlExample1Schema : String
controlExample1Schema =
    """
{
  "type": "object",
  "properties": {
    "string": {
      "type": "string"
    },
    "boolean": {
      "type": "boolean",
      "description": "Boolean description as a tooltip",
      "const": true
    },
    "number": {
      "type": "number"
    },
    "integer": {
      "type": "integer"
    },
    "date": {
      "type": "string",
      "format": "date"
    },
    "time": {
      "type": "string",
      "format": "time"
    },
    "dateTime": {
      "type": "string",
      "format": "date-time"
    },
    "enum": {
      "type": "string",
      "enum": [
        "One",
        "Two",
        "Three"
      ]
    }
  }
}
"""


controlExample1UiSchema : String
controlExample1UiSchema =
    """
{
  "type": "VerticalLayout",
  "elements": [
    {
      "type": "Control",
      "scope": "#/properties/string"
    },
    {
      "type": "Control",
      "scope": "#/properties/boolean"
    },
    {
      "type": "Control",
      "scope": "#/properties/number"
    },
    {
      "type": "Control",
      "scope": "#/properties/integer"
    },
    {
      "type": "Control",
      "scope": "#/properties/date"
    },
    {
      "type": "Control",
      "scope": "#/properties/time"
    },
    {
      "type": "Control",
      "scope": "#/properties/dateTime"
    },
    {
      "type": "Control",
      "scope": "#/properties/enum"
    }
  ]
}
"""


controlExample2Schema : String
controlExample2Schema =
    """
{
  "type": "object",
  "properties": {
    "multilineString": {
      "type": "string",
      "description": "Multiline Example"
    },
    "slider": {
      "type": "number",
      "minimum": 1,
      "maximum": 5,
      "default": 2,
      "description": "Slider Example"
    },
    "trimText": {
      "type": "string",
      "description": "Trim indicates whether the control shall grab the full width available"
    },
    "restrictText": {
      "type": "string",
      "maxLength": 5,
      "description": "Restricts the input length to the set value (in this case: 5)"
    },
    "unfocusedDescription": {
      "type": "string",
      "description": "This description is shown even when the control is not focused"
    },
    "hideRequiredAsterisk": {
      "type": "string",
      "description": "Hides the \\"*\\" symbol, when the field is required"
    },
    "toggle": {
      "type": "boolean",
      "description": "The \\"toggle\\" option renders boolean values as a toggle."
    }
  },
  "required": [
    "hideRequiredAsterisk",
    "restrictText"
  ]
}
"""


controlExample2UiSchema : String
controlExample2UiSchema =
    """
{
  "type": "VerticalLayout",
  "elements": [
    {
      "type": "Control",
      "scope": "#/properties/multilineString",
      "options": {
        "multi": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/slider",
      "options": {
        "slider": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/trimText",
      "options": {
        "trim": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/restrictText",
      "options": {
        "restrict": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/unfocusedDescription",
      "options": {
        "showUnfocusedDescription": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/hideRequiredAsterisk",
      "options": {
        "hideRequiredAsterisk": true
      }
    },
    {
      "type": "Control",
      "scope": "#/properties/toggle",
      "label": "Boolean as Toggle",
      "options": {
        "toggle": true
      }
    }
  ]
}
"""


categorizationExample1Schema : String
categorizationExample1Schema =
    """
{
  "type": "object",
  "properties": {
    "firstName": {
      "type": "string",
      "minLength": 3,
      "description": "Please enter your first name"
    },
    "secondName": {
      "type": "string",
      "minLength": 3,
      "description": "Please enter your second name"
    },
    "vegetarian": {
      "type": "boolean"
    },
    "birthDate": {
      "type": "string",
      "format": "date",
      "description": "Please enter your birth date."
    },
    "nationality": {
      "type": "string",
      "enum": [
        "DE",
        "IT",
        "JP",
        "US",
        "RU",
        "Other"
      ]
    },
    "provideAddress": {
      "type": "boolean"
    },
    "address": {
      "type": "object",
      "properties": {
        "street": {
          "type": "string"
        },
        "streetNumber": {
          "type": "string"
        },
        "city": {
          "type": "string"
        },
        "postalCode": {
          "type": "string",
          "maxLength": 5
        }
      }
    },
    "vegetarianOptions": {
      "type": "object",
      "properties": {
        "vegan": {
          "type": "boolean"
        },
        "favoriteVegetable": {
          "type": "string",
          "enum": [
            "Tomato",
            "Potato",
            "Salad",
            "Aubergine",
            "Cucumber",
            "Other"
          ]
        },
        "otherFavoriteVegetable": {
          "type": "string"
        }
      }
    }
  }
}
"""


categorizationExample1UiSchema : String
categorizationExample1UiSchema =
    """
  {
    "type": "Categorization",
    "elements": [
      {
        "type": "Category",
        "label": "Basic",
        "elements": [
          {
            "type": "HorizontalLayout",
            "elements": [
              {
                "type": "Control",
                "scope": "#/properties/firstName"
              },
              {
                "type": "Control",
                "scope": "#/properties/secondName"
              }
            ]
          },
          {
            "type": "HorizontalLayout",
            "elements": [
              {
                "type": "Control",
                "scope": "#/properties/birthDate"
              },
              {
                "type": "Control",
                "scope": "#/properties/nationality"
              }
            ]
          },
          {
            "type": "Control",
            "scope": "#/properties/provideAddress"
          },
          {
            "type": "Control",
            "scope": "#/properties/vegetarian"
          }
        ]
      },
      {
        "type": "Category",
        "label": "Address",
        "elements": [
          {
            "type": "HorizontalLayout",
            "elements": [
              {
                "type": "Control",
                "scope": "#/properties/address/properties/street"
              },
              {
                "type": "Control",
                "scope": "#/properties/address/properties/streetNumber"
              }
            ]
          },
          {
            "type": "HorizontalLayout",
            "elements": [
              {
                "type": "Control",
                "scope": "#/properties/address/properties/city"
              },
              {
                "type": "Control",
                "scope": "#/properties/address/properties/postalCode"
              }
            ]
          }
        ],
        "rule": {
          "effect": "SHOW",
          "condition": {
            "scope": "#/properties/provideAddress",
            "schema": {
              "const": true
            }
          }
        }
      },
      {
        "type": "Category",
        "label": "Additional",
        "elements": [
          {
            "type": "Control",
            "scope": "#/properties/vegetarianOptions/properties/vegan"
          },
          {
            "type": "Control",
            "scope": "#/properties/vegetarianOptions/properties/favoriteVegetable"
          },
          {
            "type": "Control",
            "scope": "#/properties/vegetarianOptions/properties/otherFavoriteVegetable",
            "rule": {
              "effect": "SHOW",
              "condition": {
                "scope": "#/properties/vegetarianOptions/properties/favoriteVegetable",
                "schema": {
                  "const": "Other"
                }
              }
            }
          }
        ],
        "rule": {
          "effect": "SHOW",
          "condition": {
            "scope": "#/properties/vegetarian",
            "schema": {
              "const": true
            }
          }
        }
      }
    ]
  }
"""


ruleExampleSchema : String
ruleExampleSchema =
    """
{
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
}
"""


ruleExampleUiSchema : String
ruleExampleUiSchema =
    """
{
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
}
"""


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


layoutExample1UiSchema : String
layoutExample1UiSchema =
    """
{
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
}
"""


layoutExample2UiSchema : String
layoutExample2UiSchema =
    """
{
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
}
"""


layoutExample3UiSchema : String
layoutExample3UiSchema =
    """
{
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
}
"""


layoutExample4UiSchema : String
layoutExample4UiSchema =
    """
{
  "type": "Group",
  "label": "My Group",
  "elements": [
    {
      "type": "HorizontalLayout",
      "elements": [
        {
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
        },
        {
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
        }
      ]
    }
  ]
}
"""


generateUiSchemaExampleSchema : String
generateUiSchemaExampleSchema =
    """
{
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
}
"""
