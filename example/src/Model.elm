module Model exposing (ExampleMsg(..), FormState, MainMsg(..), MainState, Tab(..), makeForm)

import Form exposing (Form, Msg)
import Json.Schema
import Result.Extra as Result
import Settings
import UiSchema


type alias FormState =
    { title : String
    , form : Maybe Form
    , tab : Tab
    , stringSchema : String
    , stringUiSchema : Maybe String
    , schemaError : Maybe String
    , uiSchemaError : Maybe String
    }


type Tab
    = DataTab
    | JsonSchemaTab
    | UiSchemaTab


type alias MainState =
    { forms : List FormState
    , activeForm : Int
    }


type MainMsg
    = ExampleMsg Int ExampleMsg
    | SwitchTo Int


type ExampleMsg
    = FormMsg Msg
    | EditSchema String
    | EditUiSchema String
    | SwitchTab Tab


makeForm : String -> String -> Maybe String -> FormState
makeForm title stringSchema stringUiSchema =
    let
        schema =
            Json.Schema.fromString stringSchema

        uiSchema =
            Maybe.map UiSchema.fromString stringUiSchema
    in
    case ( schema, uiSchema ) of
        ( Ok s, Nothing ) ->
            { title = title
            , form = Just <| Settings.initForm title s Nothing
            , tab = DataTab
            , stringSchema = stringSchema
            , stringUiSchema = Nothing
            , schemaError = Nothing
            , uiSchemaError = Nothing
            }

        ( Ok s, Just (Ok us) ) ->
            { title = title
            , form = Just <| Settings.initForm title s (Just us)
            , tab = DataTab
            , stringSchema = stringSchema
            , stringUiSchema = stringUiSchema
            , schemaError = Nothing
            , uiSchemaError = Nothing
            }

        ( s, us ) ->
            { title = title
            , form = Nothing
            , tab = DataTab
            , stringSchema = stringSchema
            , stringUiSchema = stringUiSchema
            , schemaError = Result.error s
            , uiSchemaError = Maybe.andThen Result.error us
            }
