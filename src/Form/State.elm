module Form.State exposing
    ( Form
    , FormState
    , Msg(..)
    , ValidateWidgets(..)
    , getErrorAt
    , initState
    , updateState
    , validateWidget
    )

import Dict exposing (Dict)
import Form.Error exposing (ErrorValue, Errors)
import Form.FieldValue as FieldValue exposing (FieldValue)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema)
import Set exposing (Set)
import UiSchema as UI
import UiSchema.Internal exposing (UiSchema)
import Validation exposing (Validation)


type alias Form =
    { schema : Schema
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


{-| Controls which widgets should show validation errors.

All - all widgets should show validation errors. Used after a form submission attempt.

Listed - only widgets in the set should show validation errors.
Widgets are added to the set when their value is updated.

-}
type ValidateWidgets
    = All
    | Listed (Set Pointer)


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
            { model | categoryFocus = Dict.insert uiState ix model.categoryFocus }


updateValidations : (Value -> Validation output) -> FormState -> FormState
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
