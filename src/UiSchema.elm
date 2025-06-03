module UiSchema exposing (UiSchema, DefOptions, fromString, decode, generate, defaultOptions)

{-| UI Schema definition and deserialization.

Documentation can be found here: <https://jsonforms.io/docs/uischema/>

@docs UiSchema, DefOptions, fromString, decode, generate, defaultOptions

-}

import Json.Decode as Decode
import Json.Schema.Definitions exposing (Schema)
import UiSchema.Internal exposing (DefOptions, decodeUiSchema, generateUiSchema)


{-| UI Schema definition
-}
type alias UiSchema =
    UiSchema.Internal.UiSchema


{-| Options for the UI Schema
-}
type alias DefOptions =
    UiSchema.Internal.DefOptions


{-| UiSchema Decoder
-}
decode : Decode.Decoder UiSchema
decode =
    decodeUiSchema


{-| Validate and produce UI Schema from JSON String
-}
fromString : String -> Result String UiSchema
fromString =
    Decode.decodeString decode >> Result.mapError Decode.errorToString


{-| Generate a UI Schema from a Schema.

The generated UI schema contains vertical layout with controls for all primitive types.

-}
generate : Schema -> UiSchema
generate =
    generateUiSchema


{-| Default options for the UI Schema
-}
defaultOptions : DefOptions
defaultOptions =
    UiSchema.Internal.defaultOptions
