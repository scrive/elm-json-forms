module Json.Schema.Form exposing
    ( State, Msg
    , init, update
    , view, submit
    , getOutput
    )

{-| Generate validating forms from JSON schemas.


# Types

@docs State, Msg


# Init/update lifecycle

@docs init, update

@docs view, submit


# Output

@docs getOutput

-}

import Form as F exposing (Msg)
import Html exposing (Html)
import Json.Schema.Definitions exposing (Schema)
import Json.Schema.Form.Error exposing (CustomErrorValue)
import Json.Schema.Form.Fields
import Form as Form
import Json.Schema.Form.Options exposing (Options)
import Json.Schema.Form.UiSchema exposing (UiSchema, generateUiSchema, defaultValues, defaultValue)
import Json.Schema.Form.Validation exposing (validation)
import Json.Encode exposing (Value)


{-| The form state.
-}
type alias State =
    { options : Options
    , schema : Schema
    , uiSchema : UiSchema
    , form : F.Form CustomErrorValue Value
    }


{-| Form messages for `update`.
-}
type alias Msg =
    F.Msg


{-| Initialize a form state with options and a schema. Use [json-tools/json-schema](https://package.elm-lang.org/packages/json-tools/json-schema/1.0.2/) to parse or build a schema.
-}
init : Options -> Schema -> Maybe UiSchema -> State
init options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefault (generateUiSchema schema) mUiSchema

        value = defaultValue schema
    in
    State options schema uiSchema <|
        Debug.log "initial form" <| F.initial (defaultValues schema uiSchema) value (validation schema value)


{-| Update the form state.
-}
update : Msg -> State -> State
update msg state =
    let
        form : F.Form CustomErrorValue Value
        form =
            F.update
                (validation state.schema (Form.getValue state.form))
                (Debug.log "message" msg)
                state.form
    in
    { state | form = Debug.log "form" form }


{-| The form fields as HTML. Use together with `submit` to submit the form.
-}
view : State -> Html Msg
view state =
    Json.Schema.Form.Fields.uiSchemaView state.options [] state.uiSchema state.schema state.form



-- Json.Schema.Form.Fields.schemaView state.options [] state.schema state.form


{-| Triggers the form to be submitted for validation.

    form [ onSubmit Json.Schema.Form.submit ]
        [ Json.Schema.Form.view state
        , button [] [ text "Submit" ]
        ]

-}
submit : Msg
submit =
    F.Submit


{-| Get the output value of the form if it validates.

    case Json.Schema.Form.getOutput state of
        Just value ->
            -- The `value` is a tree of all the fields in the form (see the
            -- `Json.Schema.Form.Value` module).
            -- Use the `Json.Schema.Form.Encode.encode` function to turn
            -- it into a JSON value.
        Nothing ->
            -- If any field in the form is not valid `getOutput` will
            -- return `Nothing`.

-}
getOutput : State -> Maybe Value
getOutput state =
    F.getOutput state.form
