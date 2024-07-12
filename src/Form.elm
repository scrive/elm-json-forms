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
import Form.Error as Error exposing (Error, ErrorValue)
import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Json.Pointer as Pointer exposing (Pointer)
import Validation exposing (Validation)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode


type alias FormState =
    { formId : String -- Unique Form ID to disambiguate element IDs for multiple forms on the sarme page
    , value : Value
    , focus : Maybe String
    , errors : Error
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
    updateValidate validation model


type alias FieldState =
    { formId : String
    , path : String
    , value : FieldValue
    , error : Maybe ErrorValue
    , hasFocus : Bool
    , disabled : Bool
    }



getField : Bool -> String -> FormState -> FieldState
getField disabled path form =
    { formId = form.formId
    , path = path
    , value = Result.toMaybe (Pointer.fromString path) |> Maybe.andThen (\pointer -> FieldValue.pointedFieldValue pointer form.value) |> Maybe.withDefault Empty
    , error = getErrorAt path form
    , hasFocus = form.focus == Just path
    , disabled = disabled
    }


type Msg
    = NoOp
    | Focus String
    | Blur String
    | Input String InputType FieldValue
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

        Focus name ->
            { model | focus = Just name }

        Blur _ ->
            let
                newModel =
                    { model | focus = Nothing }
            in
            updateValidate validation newModel

        Input name _ fieldValue ->
            let
                mPointer =
                    Result.toMaybe <| Pointer.fromString name

                newValue =
                    case mPointer of
                        Nothing ->
                            model.value

                        Just pointer ->
                            FieldValue.updateValue pointer fieldValue model.value

                newModel =
                    { model
                        | value = newValue
                    }
            in
            updateValidate validation newModel

        Submit ->
            updateValidate validation model

        Validate ->
            updateValidate validation model

        Reset value ->
            let
                newModel =
                    { model
                        | value = value
                    }
            in
            updateValidate validation newModel

        FocusCategory uiState ix ->
            let
                newModel =
                    { model | categoryFocus = Dict.insert uiState ix model.categoryFocus }
            in
            updateValidate validation newModel


updateValidate : (Value -> Validation o) -> FormState -> FormState
updateValidate validation model =
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


getErrorAt : String -> FormState -> Maybe ErrorValue
getErrorAt path model =
    List.head <|
        case Pointer.fromString path of
            Ok pointer ->
                List.filterMap
                    (\( p, e ) ->
                        if pointer == p then
                            Just e

                        else
                            Nothing
                    )
                    model.errors

            Err _ ->
                []
