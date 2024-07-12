module Form exposing
    ( Form
    , Msg
    , init
    , view
    , update
    )

{-| JSON Forms implementation with validations.

Documentation for the original TypeScript library can be found here: https://jsonforms.io/

@docs Form, Msg, init, update, view

-}

import Form.Options exposing (Options)
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
init : String -> Options -> Schema -> Maybe UiSchema -> Form
init id options schema mUiSchema =
    let
        uiSchema =
            Maybe.withDefaultLazy (always <| generateUiSchema schema) mUiSchema
    in
    Form.State.Form options schema uiSchema <|
        Form.State.initState id (defaultValue schema) (validation schema)

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
