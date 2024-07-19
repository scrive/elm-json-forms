module Main exposing (main)

import Browser
import Form exposing (Form, Msg)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Theme as Theme
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Attributes.Extra as Attrs
import Html.Events as Attrs exposing (onSubmit)
import Examples.Basic exposing (..)
import Json.Encode as Encode
import Json.Schema
import Json.Schema.Definitions exposing (Schema)
import List.Extra as List
import UiSchema exposing (UiSchema)
import Html.Extra exposing (viewMaybe)

type alias FormState =
    { title : String
    , form : Form
    , stringSchema : String
    , stringUiSchema : Maybe String
    , originalSchema : String
    , originalUiSchema : Maybe String
    , schemaError : Maybe String
    , uiSchemaError : Maybe String
    }

type alias MainState =
    { forms : List FormState
    , activeForm : Int
    }


type alias FormDefinition =
    { title : String
    , schema : Schema
    , uiSchema : Maybe UiSchema
    , stringSchema : String
    , stringUiSchema : Maybe String
    }

type MainMsg
  = ExampleMsg Int ExampleMsg
  | SwitchTo Int

type ExampleMsg
  = FormMsg Msg
  | EditSchema String
  | EditUiSchema String


formSpec : String -> String -> Maybe String -> Result String FormDefinition
formSpec title stringSchema stringUiSchema =
    let
        schema =
            Json.Schema.fromString stringSchema

        uiSchema =
            Maybe.map UiSchema.fromString stringUiSchema
    in
    case ( schema, uiSchema ) of
        ( Ok s, Nothing ) ->
            Ok
                { title = title
                , schema = s
                , uiSchema = Nothing
                , stringSchema = stringSchema
                , stringUiSchema = Nothing
                }

        ( Ok s, Just (Ok us) ) ->
            Ok
                { title = title
                , schema = s
                , uiSchema = Just us
                , stringSchema = stringSchema
                , stringUiSchema = stringUiSchema
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
            (\formDefinition ->
                case formDefinition of
                    Ok fd ->
                        { form = Form.init
                            fd.title
                            { errors = always errorString
                            , theme = Theme.tailwind
                            }
                            fd.schema
                            fd.uiSchema
                        , title = fd.title
                        , stringSchema = fd.stringSchema
                        , stringUiSchema = fd.stringUiSchema
                        , originalSchema = fd.stringSchema
                        , originalUiSchema = fd.stringUiSchema
                        , schemaError = Nothing -- TODO: fix if examples have errors
                        , uiSchemaError = Nothing -- TODO: fix if examples have errors
                        }

                    Err error ->
                        Debug.todo error
            )
            formSpecs
    , activeForm = 0
    }


update : MainMsg -> MainState -> MainState
update m state =
  case m of
    ExampleMsg i msg ->
      let
        updateForm : FormState -> FormState
        updateForm fs =
          case msg of
            FormMsg formMsg ->
              { fs | form = Form.update formMsg fs.form }

            EditSchema s ->
              case Json.Schema.fromString s of
                Ok schema ->
                 { fs
                  | stringSchema = s
                  , form = Form.setSchema schema fs.form
                  , schemaError = Nothing
                  }
                Err e ->
                  { fs
                  | stringSchema = s
                  , schemaError = Just e
                  }

            EditUiSchema s ->
                case UiSchema.fromString s of
                  Ok uiSchema ->
                    { fs
                    | stringUiSchema = Just s
                    , form = Form.setUiSchema (Just uiSchema) fs.form
                    , uiSchemaError = Nothing
                    }
                  Err e ->
                    { fs
                    | stringUiSchema = Just s
                    , uiSchemaError = Just e
                    }
      in
        { state
          | forms = List.updateAt i updateForm state.forms
        }
    SwitchTo i -> { state | activeForm = i }


view : MainState -> Html MainMsg
view state =
    div []
      [ aside [ Attrs.class "fixed w-80 top-0 left-0 p-3 bg-green" ]
        [ h1 "Examples"
        , hr [Attrs.class "my-3"] []
        , div []
          <| List.indexedMap viewLink state.forms
        ]
      , Html.map (ExampleMsg state.activeForm) <| viewMaybe viewExample (List.getAt state.activeForm state.forms)
      ]


viewLink : Int -> FormState -> Html MainMsg
viewLink i fs =
    div [class "my-1 cursor-pointer"]
      [ a [Attrs.class "hover:underline text-blue-600", Attrs.onClick (SwitchTo i)] [text fs.title]]

viewExample : FormState -> Html ExampleMsg
viewExample fs =
  div [Attrs.class "p-3 sm:ml-80"]
    [ div []
        [ h1 fs.title
        , hr [] []
        ]
    , div [ Attrs.class "flex flex-wrap -mx-2" ]
      [
        div [Attrs.class "w-1/2 px-2"]
          [ div []
            [ h2 "Form"
            , div [class "border border-black p-3 bg-gray-50"]
              [ Html.map (\m -> FormMsg m) <| Form.view fs.form]
            ]
          , div []
          [ h2 "Data"
          , viewData fs.form
          ]
        ]
      , div [Attrs.class "w-1/2 px-2"]
        [ div []
          [ h2 "JSON Schema"
          , textarea (Attrs.onInput <| EditSchema) fs.stringSchema
          , viewMaybe viewError fs.schemaError
          ]
        , div []
          [ h2 "UI Schema"
          , case fs.stringUiSchema of
              Just stringUiSchema -> textarea (Attrs.onInput <| EditUiSchema) stringUiSchema
              Nothing -> Html.span [class "text-gray-500"] [text "(empty)"]
          , viewMaybe viewError fs.uiSchemaError
          ]
        ]
      ]
    ]

viewError : String -> Html a
viewError err =
  div []
            [ h3 [class "text-red-600 font-bold mt-1"] [text "Error:"]
            , pre [class "text-sm overflow-scroll" ] [text err]
            ]

viewData : Form -> Html a
viewData form =
  let
      nErrors =
            List.length <| Error.getErrors form.state.errors

      dataText =
          Encode.encode 4 form.state.value
  in
    div []
    [ textarea (Attrs.readonly True) dataText
    , if nErrors == 0 then
          text "No errors."

        else
          text <| String.fromInt nErrors ++ " errors."
    ]


textarea : (Attribute a) -> String -> Html a
textarea attr s = Html.textarea
  [ Attrs.rows 15
  , Attrs.class "text-sm block w-full font-mono bg-gray-50"
  , Attrs.spellcheck False
  , attr
  ] [ text s ]



h1 : String -> Html a
h1 t = Html.h1 [Attrs.class "text-3xl my-5"] [ text t ]

h2 : String -> Html a
h2 t = Html.h2 [ Attrs.class "text-xl pt-4 pb-2" ] [ text t ]


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

        InvalidFormat _ ->
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
