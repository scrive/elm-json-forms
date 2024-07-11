module Main exposing (main)

import Browser
import Dict
import Form exposing (InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (FieldValue)
import Html exposing (..)
import Html.Attributes as Attrs exposing (class, disabled, style)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode as Encode exposing (bool, float, int, list, string)
import Json.Schema
import Json.Schema.Form exposing (Msg, State)
import Json.Schema.Definitions as Schema exposing (Schema)
import Json.Schema.Form.Theme as Theme exposing (Theme)
import Json.Schema.Form.UiSchema as UiSchema exposing (UiSchema)


type alias MainState =
    { forms : List State
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
    schema = Json.Schema.fromString schemaText
    uiSchema = Maybe.map UiSchema.fromString uiSchemaText

  in case (schema, uiSchema) of
    (Ok s, Nothing) ->
      Ok { title = title
      , schema = s
      , uiSchema = Nothing
      }
    (Ok s, Just (Ok us)) ->
      Ok { title = title
      , schema = s
      , uiSchema = Just us
      }
    (Err e, _) -> Err e
    (_, Just (Err e)) -> Err e


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
          , formSpec "Testing Schema" testingSchema Nothing
          ]

    in
    { forms =
      List.map (\formSpecRes ->
        case formSpecRes of
            Ok fs ->
                Json.Schema.Form.init
                        { errors = errorString
                        , formats = Dict.empty
                        , theme = Theme.tailwind
                        }
                        fs.schema
                        fs.uiSchema

            Err error ->
                Debug.todo error
      ) formSpecs
    }


update : MainMsg -> MainState -> MainState
update msg state =
    { forms = List.indexedMap (\i f -> if msg.formId == i then Json.Schema.Form.update msg.msg f else f) state.forms
    }


view : MainState -> Html MainMsg
view state =
    div [] <| List.indexedMap (\i f -> Html.map (\m -> {formId = i, msg = m}) (viewForm f)) state.forms


viewForm : State -> Html Msg
viewForm state =
    let
        anyErrors =
            not <| List.isEmpty <| Form.getErrors state.form
    in
    div []
        [ form [ onSubmit Json.Schema.Form.submit ]
            [ Json.Schema.Form.view state
            , let
                json =
                    Encode.encode 4 <| Form.getValue state.form
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


errorString : String -> ErrorValue -> String
errorString path error =
    case error of
        Empty ->
            "is a required property"

        NotConst v ->
            "Field must be equal to " ++ Encode.encode 0 v ++ "."

        InvalidString ->
            "This field is required."

        InvalidEmail ->
            "That is not a valid email address."

        InvalidFormat ->
            "That is not the correct format."

        InvalidInt ->
            "That is not a valid number."

        InvalidFloat ->
            "That is not a valid decimal number."

        InvalidBool ->
            "That is not a valid option."

        LessIntThan n ->
            "Can not be smaller than " ++ String.fromInt n ++ "."

        LessEqualIntThan n ->
            "Can not be smaller or equal than " ++ String.fromInt n ++ "."

        GreaterIntThan n ->
            "Can not be greater than " ++ String.fromInt n ++ "."

        GreaterEqualIntThan n ->
            "Can not be greater or equal than " ++ String.fromInt n ++ "."

        LessFloatThan n ->
            "Can not be smaller than " ++ String.fromFloat n ++ "."

        LessEqualFloatThan n ->
            "Can not be smaller or equal than " ++ String.fromFloat n ++ "."

        GreaterFloatThan n ->
            "Can not be greater than " ++ String.fromFloat n ++ "."

        GreaterEqualFloatThan n ->
            "Can not be greater or equal than " ++ String.fromFloat n ++ "."

        ShorterStringThan n ->
            "must NOT have fewer than " ++ String.fromInt n ++ " characters"

        LongerStringThan n ->
            "must NOT have more than " ++ String.fromInt n ++ " characters"

        NotMultipleOfInt n ->
            "Must be a multiple of " ++ String.fromInt n ++ "."

        NotMultipleOfFloat n ->
            "Must be a multiple of " ++ String.fromFloat n ++ "."

        NotIncludedIn _ ->
            "Is not a valid selection from the list."

        Unimplemented s ->
            "Unimplemented: " ++ s


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
basicExampleSchema ="""
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
basicExampleUiSchema ="""
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
controlExample1Schema ="""
{
  "type": "object",
  "properties": {
    "string": {
      "type": "string"
    },
    "boolean": {
      "type": "boolean",
      "description": "Boolean description as a tooltip"
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
controlExample1UiSchema = """
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
controlExample2Schema ="""
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
controlExample2UiSchema = """
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
