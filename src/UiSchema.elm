module UiSchema exposing (UiSchema, fromString, decode)

{-| UI Schema definition and deserialization.

Documentation can be found here: <https://jsonforms.io/docs/uischema/>

@docs UiSchema, fromString, decode, generate

-}

import Json.Decode as Decode
import Json.Schema.Definitions as Schema exposing (Schema)
import UiSchema.Internal exposing (decodeUiSchema, generateUiSchema)


{-| UI Schema definition
-}
type alias UiSchema =
    UiSchema.Internal.UiSchema


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
