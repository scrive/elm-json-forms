module Form exposing
    ( Form, Msg, init, update, view
    , setSchema, setUiSchema
    )

{-| JSON Forms implementation with validations.

Documentation for the original TypeScript library can be found here: <https://jsonforms.io/>

@docs Form, Msg, init, update, view

-}

import Form.Settings exposing (Settings)
import Form.State
import Form.Validation exposing (validation)
import Form.View
import Html exposing (Html, div)
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
init settings id schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefaultLazy (always <| generateUiSchema schema) mUiSchema
    in
    { settings = settings
    , schema = schema
    , uiSchema = uiSchema
    , state = Form.State.initState id (defaultValue schema) (validation schema)
    }


{-| Swap the Schema of an existing form

Form data is reset.

-}
setSchema : Schema -> Form -> Form
setSchema schema form =
    { form
        | schema = schema
        , state = Form.State.initState form.state.formId (defaultValue schema) (validation schema)
    }


{-| Swap the UI Schema of an existing form

Form data is preserved.

-}
setUiSchema : Maybe UiSchema -> Form -> Form
setUiSchema uiSchema form =
    { form
        | uiSchema = Maybe.withDefaultLazy (always <| generateUiSchema form.schema) uiSchema
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
