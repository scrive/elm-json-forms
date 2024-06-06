module Form exposing
    ( Msg(..), InputType(..), Form(..), FieldState
    , initial, update
    , getFocus, getErrors
    , getField, getValue
      -- , getFieldAsString, getFieldAsBool, getListIndexes
    )

{-| Simple forms made easy: A Dict implementation of the core `Json.Decode` API,
with state lifecycle and input helpers for the views.


# Types

@docs Msg, InputType, Form, FieldState


# Init/update lifecyle

@docs initial, update


# Field state accessors

@docs getFieldAsString, getFieldAsBool, getListIndexes


# Global state accessors

@docs getFocus, isSubmitted, getErrors, getOutput, getChangedFields

-}

import Dict exposing (Dict)
import Form.Error as Error exposing (Error, ErrorValue)
import Form.Field as Field exposing (FieldValue(..))
import Form.Pointer as Pointer exposing (Pointer)
import Form.Validate exposing (Validation)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Json.Schema.Definitions as Schema exposing (Schema)
import Set exposing (Set)


{-| Form to embed in your model. Type parameters are:

  - `customError` - a custom error type to extend built-in errors (set to `()` if you don't need it)
  - `output` - the type of the validation output.

-}
type Form customError
    = Form (Model customError)


{-| Private
-}
type alias Model customError =
    { value : Value
    , focus : Maybe String
    , errors : Error customError
    }


{-| Initial form state. See `Form.Field` for initial fields, and `Form.Validate` for validation.
-}
initial : Dict String FieldValue -> Value -> (Value -> Validation e output) -> Form e
initial initialValues initialValue validation =
    let
        model =
            { value = initialValue
            , focus = Nothing
            , errors = []
            }
    in
    Form (updateValidate validation model)


{-| Field state containing all necessary data for view and update,
can be retrived with `Form.getFieldAsString` or `Form.getFieldAsBool`.

  - `path` - qualified path of the field in the form, with dots for nested fields (`field.subfield`)
  - `value` - a `Maybe` of the requested type
  - `error` - a `Maybe` of the field error
  - `liveError` - same but with added logic for live validation
    (see [`getLiveErrorAt`](https://github.com/etaque/elm-form/blob/master/src/Form.elm) impl)
  - `isDirty` - if the field content has been changed since last validation
  - `isChanged` - if the field value has changed since last init/reset
  - `hasFocus` - if the field is currently focused

-}
type alias FieldState e =
    { path : String
    , value : FieldValue
    , error : Maybe (ErrorValue e)
    , hasFocus : Bool
    }



-- filterMapFieldState : (a -> Maybe b) -> FieldState e a -> Maybe (FieldState e b)
-- filterMapFieldState f fs =
--     case f fs.value of
--         Nothing -> Nothing
--         Just value ->
--             { path = fs.path
--             , value = value
--             , error = fs.error
--             , liveError = fs.liveError
--             , isDirty = fs.isDirty
--             , isChanged = fs.isChanged
--             , hasFocus = fs.hasFocus
--             }
-- {-| Get field state at path, with value as a `String`.
-- -}
-- getFieldAsString : String -> Form e -> Maybe (FieldState e String)
-- getFieldAsString path form =
--     getField path form |> filterMapFieldState (\v -> case v of
--         Bool _ -> Nothing
--         String s -> Just s)
-- {-| Get field state at path, with value as a `Bool`.
-- -}
-- getFieldAsBool : String -> Form e -> FieldState e Bool
-- getFieldAsBool =
--     getField getBoolAt


getValue : Form e -> Value
getValue (Form form) =
    form.value



-- TODO: re-implement


getValue_ : Pointer -> Value -> Maybe FieldValue
getValue_ pointer value =
    case pointer of
        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok dict ->
                    Maybe.andThen (getValue_ ps) <| Dict.get key dict

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



-- getField : String -> Form e -> Maybe (FieldState e)
-- getField path form = Result.toMaybe (Pointer.fromString path)
--     |> Maybe.map (\pointer ->
--             { path = path
--             , value = getValue_ pointer (getValue form)
--             , error = getErrorAt path form
--             , hasFocus = getFocus form == Just path
--             }
--     )


getField : String -> Form e -> FieldState e
getField path form =
    { path = path
    , value = Result.toMaybe (Pointer.fromString path) |> Maybe.andThen (\pointer -> getValue_ pointer (getValue form)) |> Maybe.withDefault Empty
    , error = getErrorAt path form
    , hasFocus = getFocus form == Just path
    }



-- {-| return a list of indexes so one can build qualified names of fields in list.
-- -}
-- getListIndexes : String -> Form e -> List Int
-- getListIndexes path (F model) =
--     let
--         length =
--             getFieldAt path model
--                 |> Maybe.map (Tree.asList >> List.length)
--                 |> Maybe.withDefault 0
--     in
--     List.range 0 (length - 1)


{-| Form messages for `update`.
-}
type Msg
    = NoOp
    | Focus String
    | Blur String
    | Input String InputType FieldValue
      -- | Append String
      -- | RemoveItem String Int
    | Submit
    | Validate
    | Reset Value


{-| Input types to determine live validation behaviour.
-}
type InputType
    = Text
    | Textarea
    | Select
    | Radio
    | Checkbox


{-| Update form state with the given message
-}
update : (Value -> Validation e output) -> Msg -> Form e -> Form e
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

        -- Append listName ->
        --     let
        --         listFields =
        --             getFieldAt listName model
        --                 |> Maybe.map Tree.asList
        --                 |> Maybe.withDefault []
        --         newListFields =
        --             listFields ++ [ Tree.Value Field.EmptyField ]
        --         newModel =
        --             { model
        --                 | fields = setFieldAt listName (Tree.List newListFields) model
        --             }
        --     in
        --     F newModel
        -- RemoveItem listName index ->
        --     let
        --         listFields =
        --             getFieldAt listName model
        --                 |> Maybe.map Tree.asList
        --                 |> Maybe.withDefault []
        --         fieldNamePattern =
        --             listName ++ String.fromInt index
        --         filterChangedFields =
        --             Set.filter (not << String.startsWith fieldNamePattern)
        --         filterOriginalValue =
        --             Dict.filter (\c _ -> not <| String.startsWith fieldNamePattern c)
        --         newListFields =
        --             List.take index listFields ++ List.drop (index + 1) listFields
        --         newModel =
        --             { model
        --                 | fields = setFieldAt listName (Tree.List newListFields) model
        --                 , changedFields = filterChangedFields model.changedFields
        --                 , originalValues = filterOriginalValue model.originalValues
        --             }
        --     in
        --     F (updateValidate validation newModel)
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


updateValue : Pointer -> FieldValue -> Value -> Value
updateValue pointer new value =
    case pointer of
        "properties" :: key :: [] ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        case Field.asValue new of
                            Nothing ->
                                Dict.remove key o

                            Just v ->
                                Dict.insert key v o

                Err e ->
                    value

        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        Dict.insert key (updateValue ps new (Maybe.withDefault Encode.null <| Dict.get key o)) o

                Err e ->
                    value

        [] ->
            Maybe.withDefault Encode.null <| Field.asValue new

        _ ->
            value


updateValidate : (Value -> Validation e o) -> Model e -> Model e
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


{-| Get list of errors on qualified paths.
-}
getErrors : Form e -> List ( String, Error.ErrorValue e )
getErrors (Form model) =
    List.map (\( p, e ) -> ( Pointer.toString p, e )) model.errors


getErrorAt : String -> Form e -> Maybe (ErrorValue e)
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


{-| Return currently focused field, if any.
-}
getFocus : Form e -> Maybe String
getFocus (Form model) =
    model.focus
