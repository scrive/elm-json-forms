module UiSchema exposing
    ( UiSchema
    , fromString
    )

{-| UI Schema definition and deserialization.

Documentation can be found here: https://jsonforms.io/docs/uischema/

@docs UiSchema, fromString

-}

import UiSchema.Internal exposing (decodeUiSchema)
import Json.Decode as Decode

{-| UI Schema definition
-}
type alias UiSchema = UiSchema.Internal.UiSchema

{-| Validate and produce UI Schema from JSON String
-}
fromString : String -> Result String UiSchema
fromString =
    Decode.decodeString decodeUiSchema >> Result.mapError Decode.errorToString
