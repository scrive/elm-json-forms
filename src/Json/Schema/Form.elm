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

import Form exposing (Form(..), Msg)
import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Json.Schema.Definitions exposing (Schema)
import Json.Schema.Form.Error exposing (CustomErrorValue)
import Json.Schema.Form.Fields
import Json.Schema.Form.Options exposing (Options)
import Json.Schema.Form.UiSchema exposing (UiSchema, defaultValue, defaultValues, generateUiSchema)
import Json.Schema.Form.Validation exposing (validation)


{-| The form state.
-}
type alias State =
    { options : Options
    , schema : Schema
    , uiSchema : UiSchema
    , form : Form CustomErrorValue
    }


{-| Form messages for `update`.
-}
type alias Msg =
    Form.Msg


{-| Initialize a form state with options and a schema. Use [json-tools/json-schema](https://package.elm-lang.org/packages/json-tools/json-schema/1.0.2/) to parse or build a schema.
-}
init : Options -> Schema -> Maybe UiSchema -> State
init options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefault (generateUiSchema schema) mUiSchema

        value =
            defaultValue schema
    in
    State options schema uiSchema <|
        Debug.log "initial form" <|
            Form.initial (defaultValues schema uiSchema) value (validation schema)


{-| Update the form state.
-}
update : Msg -> State -> State
update msg state =
    let
        form : Form CustomErrorValue
        form =
            Form.update
                (validation state.schema)
                (Debug.log "message" msg)
                state.form
    in
    { state | form = (\( _, a ) -> a) <| Debug.log "form" ( Encode.encode 0 <| Form.getValue form, form ) }


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
    Form.Submit


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
    let
        form = case state.form of
            Form f -> f -- wtf elm
    in
        if form.errors == [] then Just form.value else Nothing
