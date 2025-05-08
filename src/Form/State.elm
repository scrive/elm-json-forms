module Form.State exposing
    ( FieldState
    , Form
    , FormState
    , Msg(..)
    , ValidateWidgets(..)
    , fieldState
    , getErrorAt
    , initState
    , updateState
    , validateWidget
    )

import Dict exposing (Dict)
import Form.Error exposing (ErrorValue, Errors)
import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Form.Settings exposing (Settings)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema)
import Set exposing (Set)
import UiSchema as UI
import UiSchema.Internal exposing (UiSchema)
import Validation exposing (Validation)


type alias Form =
    { settings : Settings
    , schema : Schema
    , uiSchema : UiSchema
    , uiSchemaIsGenerated : Bool
    , state : FormState
    , defaultOptions : UI.DefOptions
    }


type alias FormState =
    { formId : String -- Unique Form ID to disambiguate element IDs for multiple forms on the sarme page
    , value : Value
    , focus : Maybe Pointer
    , errors : Errors
    , categoryFocus : Dict (List Int) Int
    , validateWidgets : ValidateWidgets
    }


type ValidateWidgets
    = All
    | Listed (Set Pointer)


type alias FieldState =
    { formId : String
    , pointer : Pointer
    , value : FieldValue
    , error : Maybe ErrorValue
    , hasFocus : Bool
    , disabled : Bool
    , required : Bool
    }


type Msg
    = Focus Pointer
    | Input Pointer FieldValue
    | FocusCategory (List Int) Int
    | ValidateAll


validateWidgetsMap : (Set Pointer -> Set Pointer) -> ValidateWidgets -> ValidateWidgets
validateWidgetsMap f vw =
    case vw of
        All ->
            All

        Listed set ->
            Listed (f set)


validateWidget : Pointer -> ValidateWidgets -> Bool
validateWidget pointer vw =
    case vw of
        All ->
            True

        Listed set ->
            Set.member pointer set


initState : String -> Value -> (Value -> Validation output) -> FormState
initState formId initialValue validation =
    let
        model =
            { formId = formId
            , value = initialValue
            , focus = Nothing
            , errors = []
            , categoryFocus = Dict.empty
            , validateWidgets = Listed Set.empty
            }
    in
    updateValidations validation model


fieldState : Bool -> Bool -> Pointer -> FormState -> FieldState
fieldState disabled required pointer form =
    { formId = form.formId
    , pointer = pointer
    , value = Maybe.withDefault (String "") <| FieldValue.pointedFieldValue pointer form.value
    , error = getErrorAt pointer form.errors
    , hasFocus = form.focus == Just pointer
    , disabled = disabled
    , required = required
    }


updateState : (Value -> Validation output) -> Msg -> FormState -> FormState
updateState validation msg model =
    case msg of
        Focus pointer ->
            { model
                | focus = Just pointer
            }

        ValidateAll ->
            { model | validateWidgets = All }

        Input pointer fieldValue ->
            updateValidations validation
                { model
                    | value = FieldValue.updateValue pointer fieldValue model.value
                    , validateWidgets = validateWidgetsMap (Set.insert pointer) model.validateWidgets
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
