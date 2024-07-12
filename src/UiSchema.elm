module UiSchema exposing
    ( UiSchema
    , fromString
    )

import UiSchema.Internal exposing (decodeUiSchema)
import Json.Decode as Decode

type alias UiSchema = UiSchema.Internal.UiSchema

fromString : String -> Result String UiSchema
fromString =
    Decode.decodeString decodeUiSchema >> Result.mapError Decode.errorToString
