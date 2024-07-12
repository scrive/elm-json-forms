module Form exposing
    ( FieldState
    , FormState
    , InputType(..)
    , Msg(..)
    , getErrors
    , getField
    , getPointedValue
    , initial
    , update
    )

import Dict exposing (Dict)
import Form.Error as Error exposing (Error, ErrorValue)
import Form.Field as Field exposing (FieldValue(..))
import Form.Pointer as Pointer exposing (Pointer)
import Form.Validate exposing (Validation)
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


toFieldValue : Value -> Maybe FieldValue
toFieldValue value =
    case
        Decode.decodeValue
            (Decode.oneOf
                [ Decode.map Field.Int Decode.int
                , Decode.map Field.Number Decode.float
                , Decode.map Field.String Decode.string
                , Decode.map Field.Bool Decode.bool
                ]
            )
            value
    of
        Ok fv ->
            Just fv

        Err _ ->
            Nothing


getPointedFieldValue : Pointer -> Value -> Maybe FieldValue
getPointedFieldValue pointer value =
    Maybe.andThen toFieldValue (getPointedValue pointer value)


getPointedValue : Pointer -> Value -> Maybe Value
getPointedValue pointer value =
    case pointer of
        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok dict ->
                    Maybe.andThen (getPointedValue ps) <| Dict.get key dict

                Err _ ->
                    Nothing

        [] ->
            Just value

        _ ->
            Nothing


getField : Bool -> String -> FormState -> FieldState
getField disabled path form =
    { formId = form.formId
    , path = path
    , value = Result.toMaybe (Pointer.fromString path) |> Maybe.andThen (\pointer -> getPointedFieldValue pointer form.value) |> Maybe.withDefault Empty
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
                            updateValue pointer fieldValue model.value

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


updateValue : Pointer -> FieldValue -> Value -> Value
updateValue pointer new value =
    case pointer of
        "properties" :: key :: [] ->
            Encode.dict identity identity <|
                case ( Decode.decodeValue (Decode.dict Decode.value) value, Field.asValue new ) of
                    ( Ok o, Nothing ) ->
                        Dict.remove key o

                    ( Ok o, Just v ) ->
                        Dict.insert key v o

                    ( Err _, Nothing ) ->
                        Dict.empty

                    ( Err _, Just v ) ->
                        Dict.singleton key v

        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        Dict.insert key (updateValue ps new (Maybe.withDefault Encode.null <| Dict.get key o)) o

                Err _ ->
                    Encode.dict identity identity <| Dict.singleton key (updateValue ps new Encode.null)

        [] ->
            Maybe.withDefault Encode.null <| Field.asValue new

        _ ->
            value


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


getErrors : FormState -> List ( String, Error.ErrorValue )
getErrors model =
    List.map (\( p, e ) -> ( Pointer.toString p, e )) model.errors


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
