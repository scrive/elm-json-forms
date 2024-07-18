module UiSchema exposing (UiSchema, fromString, decode)

{-| UI Schema definition and deserialization.

Documentation can be found here: <https://jsonforms.io/docs/uischema/>

@docs UiSchema, fromString, decode

-}

import Json.Decode as Decode
import UiSchema.Internal exposing (decodeUiSchema)


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
