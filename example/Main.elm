module Main exposing (main)

import Browser
import Dict
import Form exposing (InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (FieldValue)
import Form.Input exposing (Input)
import Form.Validate
import Html exposing (..)
import Html.Attributes as Attrs exposing (class, disabled, style)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode as Encode exposing (bool, float, int, list, string)
import Json.Schema
import Json.Schema.Builder exposing (..)
import Json.Schema.Definitions
import Json.Schema.Form exposing (Msg, State)
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Theme as Theme exposing (Theme)
import Json.Schema.Form.UiSchema as UiSchema
import Regex


type alias MainState =
    { forms : List State
    }


main : Program () MainState Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : MainState
init =
    let
        schemas =
            [ basicExampleSchema
            ]

        uiSchemas =
            [ basicExampleUiSchema
            ]
    in
    case ( basicExampleSchema, basicExampleUiSchema ) of
        ( Ok schema, Ok uiSchema ) ->
            { forms =
                [ Json.Schema.Form.init
                    { errors = errorString
                    , formats = Dict.empty
                    , theme = Theme.tailwind
                    }
                    schema
                    (Just uiSchema)
                ]
            }

        ( Err error, _ ) ->
            Debug.todo error

        ( _, Err error ) ->
            Debug.todo error


update : Msg -> MainState -> MainState
update msg state =
    { forms = List.map (Json.Schema.Form.update msg) state.forms
    }


view : MainState -> Html Msg
view state =
    div [] <| List.map viewForm state.forms


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


schema_json : Result String Json.Schema.Definitions.Schema
schema_json =
    Json.Schema.fromString """
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


basicExampleSchema : Result String Json.Schema.Definitions.Schema
basicExampleSchema =
    Json.Schema.fromString """
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


basicExampleUiSchema : Result String UiSchema.UiSchema
basicExampleUiSchema =
    UiSchema.fromString """
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
