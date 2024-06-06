module Json.Schema.Form exposing
    ( Msg
    , State
    , getOutput
    , init
    , submit
    , update
    , view
    )

import Form exposing (Form(..), Msg)
import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Json.Schema.Definitions exposing (Schema)
import Json.Schema.Form.Fields
import Json.Schema.Form.Options exposing (Options)
import Json.Schema.Form.UiSchema exposing (UiSchema, defaultValue, generateUiSchema)
import Json.Schema.Form.Validation exposing (validation)


type alias State =
    { options : Options
    , schema : Schema
    , uiSchema : UiSchema
    , form : Form
    }


type alias Msg =
    Form.Msg


init : Options -> Schema -> Maybe UiSchema -> State
init options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefault (generateUiSchema schema) mUiSchema
    in
    State options schema uiSchema <|
        Form.initial (defaultValue schema) (validation schema)


update : Msg -> State -> State
update msg state =
    let
        form : Form
        form =
            Form.update
                (validation state.schema)
                (Debug.log "message" msg)
                state.form
    in
    { state | form = (\( _, a ) -> a) <| Debug.log "form" ( Encode.encode 0 <| Form.getValue form, form ) }


view : State -> Html Msg
view state =
    Json.Schema.Form.Fields.uiSchemaView state.options [] state.uiSchema state.schema state.form


submit : Msg
submit =
    Form.Submit


getOutput : State -> Maybe Value
getOutput state =
    let
        form =
            case state.form of
                Form f ->
                    f
    in
    if form.errors == [] then
        Just form.value

    else
        Nothing
