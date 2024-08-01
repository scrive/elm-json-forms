module Form exposing (Form, Msg, init, update, view, getValue, getSchema, getUiSchema, getErrors, setSchema, setUiSchema)

{-| JSON Forms implementation with validations.

Documentation for the original TypeScript library can be found here: <https://jsonforms.io/>

@docs Form, Msg, init, update, view, getValue, getSchema, getUiSchema, getErrors, setSchema, setUiSchema

-}

import Form.Error as Error
import Form.Settings exposing (Settings)
import Form.State
import Form.Validation exposing (validation)
import Form.View
import Html exposing (Html, div)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema)
import Maybe.Extra as Maybe
import UiSchema.Internal exposing (UiSchema, defaultValue, generateUiSchema)


{-| Main form type
-}
type alias Form =
    Form.State.Form


{-| Form messages
-}
type alias Msg =
    Form.State.Msg


{-| Initialize form state
-}
init : Settings -> String -> Schema -> Maybe UiSchema -> Form
init settings id schema uiSchema =
    { settings = settings
    , schema = schema
    , uiSchema = Maybe.withDefaultLazy (always <| generateUiSchema schema) uiSchema
    , uiSchemaIsGenerated = uiSchema == Nothing
    , state = Form.State.initState id (defaultValue schema) (validation schema)
    }


{-| Swap the Schema of an existing form

Form data is reset. UI Schema is re-generated if it was auto-generated in the first place.

-}
setSchema : Schema -> Form -> Form
setSchema schema form =
    { form
        | schema = schema
        , uiSchema =
            if form.uiSchemaIsGenerated then
                generateUiSchema schema

            else
                form.uiSchema
        , state = Form.State.initState form.state.formId (defaultValue schema) (validation schema)
    }


{-| Swap the UI Schema of an existing form

Form data is preserved.

-}
setUiSchema : Maybe UiSchema -> Form -> Form
setUiSchema uiSchema form =
    { form
        | uiSchema = Maybe.withDefaultLazy (always <| generateUiSchema form.schema) uiSchema
        , uiSchemaIsGenerated = uiSchema == Nothing
    }


{-| View the form
-}
view : Form -> Html Msg
view form =
    div [] <| Form.View.view form { uiPath = [], disabled = False, uiSchema = form.uiSchema }


{-| Update the form
-}
update : Msg -> Form -> Form
update msg form =
    { form
        | state =
            Form.State.updateState
                (validation form.schema)
                msg
                form.state
    }


{-| Get the current form value.

The returned value may not be conforming to the JSON Schema if the
list of validation errors returned by `getErrors` is non-empty.

-}
getValue : Form -> Value
getValue form =
    form.state.value


{-| Get the current Schema
-}
getSchema : Form -> Schema
getSchema form =
    form.schema


{-| Get the current UI Schema
-}
getUiSchema : Form -> UiSchema
getUiSchema form =
    form.uiSchema


{-| Get all form errors as a list.

Empty list is returned if the form contains no errors.

-}
getErrors : Form -> List ( Pointer, Error.ErrorValue )
getErrors form =
    List.map (\( p, e ) -> ( p, e )) form.state.errors
