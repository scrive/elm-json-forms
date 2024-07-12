module Form.State exposing
    ( FieldState
    , Form
    , FormState
    , Msg(..)
    , fieldState
    , init
    , initial
    , update
    )

import Dict exposing (Dict)
import Form.Error exposing (ErrorValue, Errors)
import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Form.Options exposing (Options)
import Form.Validation exposing (validation)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema)
import Maybe.Extra as Maybe
import UiSchema.Internal exposing (UiSchema, defaultValue, generateUiSchema)
import Validation exposing (Validation)


type alias Form =
    { options : Options
    , schema : Schema
    , uiSchema : UiSchema
    , state : FormState
    }


init : String -> Options -> Schema -> Maybe UiSchema -> Form
init id options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefaultLazy (always <| generateUiSchema schema) mUiSchema
    in
    Form options schema uiSchema <|
        initial id (defaultValue schema) (validation schema)


type alias FormState =
    { formId : String -- Unique Form ID to disambiguate element IDs for multiple forms on the sarme page
    , value : Value
    , focus : Maybe Pointer
    , errors : Errors
    , categoryFocus : Dict (List Int) Int
    }


type alias FieldState =
    { formId : String
    , pointer : Pointer
    , value : FieldValue
    , error : Maybe ErrorValue
    , hasFocus : Bool
    , disabled : Bool
    }


type Msg
    = NoOp
    | Focus Pointer
    | Blur
    | Input Pointer FieldValue
    | Submit
    | Validate
    | Reset Value
    | FocusCategory (List Int) Int


initial : String -> Value -> (Value -> Validation output) -> FormState
initial formId initialValue validation =
    let
        model =
            { formId = formId
            , value = initialValue
            , focus = Nothing
            , errors = []
            , categoryFocus = Dict.empty
            }
    in
    updateValidations validation model


fieldState : Bool -> Pointer -> FormState -> FieldState
fieldState disabled pointer form =
    { formId = form.formId
    , pointer = pointer
    , value = Maybe.withDefault Empty <| FieldValue.pointedFieldValue pointer form.value
    , error = getErrorAt pointer form.errors
    , hasFocus = form.focus == Just pointer
    , disabled = disabled
    }


update : Msg -> Form -> Form
update msg form =
    { form
        | state =
            updateState
                (validation form.schema)
                msg
                form.state
    }


updateState : (Value -> Validation output) -> Msg -> FormState -> FormState
updateState validation msg model =
    case msg of
        NoOp ->
            model

        Focus pointer ->
            { model | focus = Just pointer }

        Blur ->
            updateValidations validation { model | focus = Nothing }

        Input pointer fieldValue ->
            updateValidations validation
                { model | value = FieldValue.updateValue pointer fieldValue model.value }

        Submit ->
            updateValidations validation model

        Validate ->
            updateValidations validation model

        Reset value ->
            updateValidations validation
                { model
                    | value = value
                }

        FocusCategory uiState ix ->
            updateValidations validation { model | categoryFocus = Dict.insert uiState ix model.categoryFocus }


updateValidations : (Value -> Validation o) -> FormState -> FormState
updateValidations validation model =
    case validation model.value of
        Ok _ ->
            { model
                | errors =
                    []
            }

        Err error ->
            { model
                | errors =
                    error
            }


getErrorAt : Pointer -> Errors -> Maybe ErrorValue
getErrorAt pointer =
    List.head
        << List.filterMap
            (\( p, e ) ->
                if pointer == p then
                    Just e

                else
                    Nothing
            )
