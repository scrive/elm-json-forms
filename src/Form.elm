module Form exposing
    ( Form
    , Msg
    , init
    , update
    , view
    )

import Form.State exposing (FormState, Msg)
import Html exposing (Html, div)
import Json.Encode as Encode
import Json.Schema.Definitions exposing (Schema)
import Form.View
import Form.Options exposing (Options)
import UiSchema exposing (UiSchema, defaultValue, generateUiSchema)
import Form.Validation exposing (validation)
import Maybe.Extra as Maybe
import Form.Error exposing (ErrorValue)


type alias Form =
    { options : Options
    , schema : Schema
    , uiSchema : UiSchema
    , state : FormState
    }


type alias Msg =
    Form.State.Msg


init : String -> Options -> Schema -> Maybe UiSchema -> Form
init id options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefaultLazy (\_ -> generateUiSchema schema) mUiSchema
    in
    Form options schema uiSchema <|
        Form.State.initial id (defaultValue schema) (validation schema)


update : Msg -> Form -> Form
update msg form =
    let
        formState : FormState
        formState =
            Form.State.update
                (validation form.schema)
                msg
                form.state
    in
    { form | state = (\( _, a ) -> a) ( Encode.encode 0 formState.value, formState ) }


view : Form -> Html Msg
view form =
    div [] <| Form.View.view form.options { uiPath = [], disabled = False } form.uiSchema form.schema form.state
