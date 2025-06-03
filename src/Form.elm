module Form exposing (Form, Msg, init, defaultOptions, update, widget, viewWidget, errorString, getRawValue, getSubmitValue, getSchema, getUiSchema, getErrors, setSchema, setUiSchema, validateAllFieldsMsg)

{-| JSON Forms implementation with validations.

Documentation for the original TypeScript library can be found here: <https://jsonforms.io/>

@docs Form, Msg, init, defaultOptions, update, widget, viewWidget, errorString, getRawValue, getSubmitValue, getSchema, getUiSchema, getErrors, setSchema, setUiSchema, validateAllFieldsMsg

-}

import Form.Error as Error
import Form.State
import Form.Validation exposing (validate)
import Form.Widget
import Form.Widget.Generate
import Form.Widget.View
import Html exposing (Html)
import Json.Decode exposing (Value)
import Json.Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema)
import Maybe.Extra as Maybe
import UiSchema as UI
import UiSchema.Internal exposing (UiSchema, defaultValue, generateUiSchema)


{-| Main form type
-}
type alias Form =
    Form.State.Form


{-| Form messages
-}
type alias Msg =
    Form.State.Msg


{-| Enable form validations for all fields.

Until this message is triggered, fields are validated only after input.

-}
validateAllFieldsMsg : Msg
validateAllFieldsMsg =
    Form.State.ValidateAll


{-| Initialize form state.

Supplying anything other than [`defaultOptions`](#defaultOptions) into the `init` function
causes the resulting form to differ from json-forms.io specification. These differences
should be documented.

-}
init : UI.DefOptions -> String -> Schema -> Maybe UiSchema -> Form
init options id schema uiSchema =
    { schema = schema
    , uiSchema = Maybe.withDefaultLazy (\() -> generateUiSchema schema) uiSchema
    , uiSchemaIsGenerated = uiSchema == Nothing
    , state = Form.State.initState id (defaultValue schema) (validate schema)
    , defaultOptions = options
    }


{-| Default element options.
-}
defaultOptions : UI.DefOptions
defaultOptions =
    UI.defaultOptions


{-| Render the form into an abstract view representation.

This representation can in turn be rendered into HTML by [`viewWidget`](#viewWidget),
or by a custom function.

Widget type is documented in the [`Form.Widget`](Form-Widget) module.

-}
widget : Form -> Form.Widget.Widget
widget =
    Form.Widget.Generate.widget


{-| View a widget.

This function can be used as a template for your own view function.

Widget type is documented in the [`Form.Widget`](Form-Widget) module.

-}
viewWidget : Form.Widget.Widget -> Html Msg
viewWidget =
    Form.Widget.View.viewWidget


{-| Convert an error value to a string.

This function can be used as a template for your own error messages.

Error value is documented in the [`Form.Error`](Form-Error) module.

-}
errorString : Error.ErrorValue -> String
errorString =
    Form.Widget.View.errorString


{-| Swap the Schema of an existing form.

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
        , state = Form.State.initState form.state.formId (defaultValue schema) (validate schema)
    }


{-| Swap the UI Schema of an existing form.

Form data is preserved.

-}
setUiSchema : Maybe UiSchema -> Form -> Form
setUiSchema uiSchema form =
    { form
        | uiSchema = Maybe.withDefaultLazy (\() -> generateUiSchema form.schema) uiSchema
        , uiSchemaIsGenerated = uiSchema == Nothing
    }


{-| Update the form
-}
update : Msg -> Form -> Form
update msg form =
    { form
        | state =
            Form.State.updateState
                (validate form.schema)
                msg
                form.state
    }


{-| Get the current form value.

The returned value reflects the current form contents.
It is not normalized, and may not be conforming to the JSON Schema.
To get a normalized value conforming to the JSON Schema, use [`getSubmitValue`](#getSubmitValue).

-}
getRawValue : Form -> Value
getRawValue form =
    form.state.value


{-| Get the current form value.

The value is present only if form validation passes with no errors.

-}
getSubmitValue : Form -> Maybe Value
getSubmitValue form =
    validate form.schema form.state.value
        |> Result.toMaybe


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
