module Form exposing
    ( FieldState
    , Form(..)
    , InputType(..)
    , Msg(..)
    , getErrors
    , getField
    , getFocus
    , getValue
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
import Json.Schema.Definitions as Schema exposing (Schema)
import Set exposing (Set)


type Form
    = Form Model


type alias Model =
    { value : Value
    , focus : Maybe String
    , errors : Error
    , categoryFocus : Dict (List Int) Int
    }


initial : Value -> (Value -> Validation output) -> Form
initial initialValue validation =
    let
        model =
            { value = initialValue
            , focus = Nothing
            , errors = []
            , categoryFocus = Dict.empty
            }
    in
    Form (updateValidate validation model)


type alias FieldState =
    { path : String
    , value : FieldValue
    , error : Maybe ErrorValue
    , hasFocus : Bool
    }


getValue : Form -> Value
getValue (Form form) =
    form.value


getPointedValue : Pointer -> Value -> Maybe FieldValue
getPointedValue pointer value =
    case pointer of
        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok dict ->
                    Maybe.andThen (getPointedValue ps) <| Dict.get key dict

                Err _ ->
                    Nothing

        [] ->
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

        _ ->
            Nothing


getField : String -> Form -> FieldState
getField path form =
    { path = path
    , value = Result.toMaybe (Pointer.fromString path) |> Maybe.andThen (\pointer -> getPointedValue pointer (getValue form)) |> Maybe.withDefault Empty
    , error = getErrorAt path form
    , hasFocus = getFocus form == Just path
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


update : (Value -> Validation output) -> Msg -> Form -> Form
update validation msg (Form model) =
    case msg of
        NoOp ->
            Form model

        Focus name ->
            let
                newModel =
                    { model | focus = Just name }
            in
            Form newModel

        Blur name ->
            let
                newModel =
                    { model | focus = Nothing }
            in
            Form (updateValidate validation newModel)

        Input name inputType fieldValue ->
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
                        | value = Debug.log "Update input value" newValue
                    }
            in
            Form (updateValidate validation newModel)

        Submit ->
            Form (updateValidate validation model)

        Validate ->
            Form (updateValidate validation model)

        Reset value ->
            let
                newModel =
                    { model
                        | value = value
                    }
            in
            Form (updateValidate validation newModel)

        FocusCategory uiPath ix ->
            let
                newModel =
                    { model | categoryFocus = Dict.insert uiPath ix model.categoryFocus }
            in Form (updateValidate validation newModel)


updateValue : Pointer -> FieldValue -> Value -> Value
updateValue pointer new value =
    case pointer of
        "properties" :: key :: [] ->
            Encode.dict identity identity <| case (Decode.decodeValue (Decode.dict Decode.value) value, Field.asValue new) of
                (Ok o, Nothing) -> Dict.remove key o
                (Ok o, Just v) -> Dict.insert key v o
                (Err _, Nothing) -> Dict.empty
                (Err _, Just v) -> Dict.singleton key v

        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        Dict.insert key (updateValue ps new (Maybe.withDefault Encode.null <| Dict.get key o)) o

                Err e ->
                    Encode.dict identity identity <| Dict.singleton key (updateValue ps new Encode.null)

        [] ->
            Maybe.withDefault Encode.null <| Field.asValue new

        _ ->
            value


updateValidate : (Value -> Validation o) -> Model -> Model
updateValidate validation model =
    case validation model.value of
        Ok output ->
            { model
                | errors =
                    []
            }

        Err error ->
            { model
                | errors =
                    error
            }


getErrors : Form -> List ( String, Error.ErrorValue )
getErrors (Form model) =
    List.map (\( p, e ) -> ( Pointer.toString p, e )) model.errors


getErrorAt : String -> Form -> Maybe ErrorValue
getErrorAt path (Form model) =
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


getFocus : Form -> Maybe String
getFocus (Form model) =
    model.focus
