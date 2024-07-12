module Form exposing
    ( FieldState
    , FormState
    , InputType(..)
    , Msg(..)
    , getField
    , initial
    , update
    )

import Dict exposing (Dict)
import Form.Error as Error exposing (ErrorValue, Errors)
import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Json.Pointer as Pointer exposing (Pointer)
import Validation exposing (Validation)


type alias FormState =
    { formId : String -- Unique Form ID to disambiguate element IDs for multiple forms on the sarme page
    , value : Value
    , focus : Maybe Pointer
    , errors : Errors
    , categoryFocus : Dict (List Int) Int
    }


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


type alias FieldState =
    { formId : String
    , pointer : Pointer
    , value : FieldValue
    , error : Maybe ErrorValue
    , hasFocus : Bool
    , disabled : Bool
    }


getField : Bool -> Pointer -> FormState -> FieldState
getField disabled pointer form =
    { formId = form.formId
    , pointer = pointer
    , value = Maybe.withDefault Empty <| FieldValue.pointedFieldValue pointer form.value
    , error = getErrorAt pointer form.errors
    , hasFocus = form.focus == Just pointer
    , disabled = disabled
    }


type Msg
    = NoOp
    | Focus Pointer
    | Blur Pointer -- TODO: remove pointer here?
    | Input Pointer InputType FieldValue
    | Submit
    | Validate
    | Reset Value
    | FocusCategory (List Int) Int


type InputType
    = Text
    | Textarea
    | Select
    | Radio
    | Checkbox


update : (Value -> Validation output) -> Msg -> FormState -> FormState
update validation msg model =
    case msg of
        NoOp ->
            model

        Focus pointer ->
            { model | focus = Just pointer }

        Blur _ ->
            updateValidations validation { model | focus = Nothing }

        Input pointer _ fieldValue ->
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
