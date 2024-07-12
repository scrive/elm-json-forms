module Json.Schema.Form.Format exposing
    ( Format
    , init
    , withAutocomplete
    , withAutocompleteOff
    , withAutocompleteOn
    , withInputType
    , withPlaceholder
    , withPrefix
    , withSuffix
    , withValidation
    )

import Form.Field exposing (FieldValue)
import Form.Input exposing (Input)
import Form.Validate exposing (Validation)


{-| A custom format.
-}
type alias Format =
    { prefix : Maybe String
    , suffix : Maybe String
    , placeholder : Maybe String
    , autocomplete : Maybe String
    , inputType : Maybe String
    , validation : String -> Validation String
    }


{-| Initialize a new format with default values.
-}
init : Format
init =
    { prefix = Nothing
    , suffix = Nothing
    , placeholder = Nothing
    , autocomplete = Nothing
    , inputType = Nothing
    , validation = Form.Validate.succeed
    }


{-| A short label that is displayed at the beginning of the input field.
-}
withPrefix : String -> Format -> Format
withPrefix str format =
    { format | prefix = Just str }


{-| A short label that is displayed at the end of the input field.
-}
withSuffix : String -> Format -> Format
withSuffix str format =
    { format | suffix = Just str }


{-| A short hint that describes the expected value of the field.
-}
withPlaceholder : String -> Format -> Format
withPlaceholder str format =
    { format | placeholder = Just str }


{-| The browser is not permitted to automatically enter or select a value for this field.
-}
withAutocompleteOff : Format -> Format
withAutocompleteOff format =
    { format | autocomplete = Just "off" }


{-| The browser is allowed to automatically complete the input. No guidance is provided as to the type of data expected in the field, so the browser may use its own judgement.
-}
withAutocompleteOn : Format -> Format
withAutocompleteOn format =
    { format | autocomplete = Just "on" }


{-| The browser is allowed to automatically complete the input. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete> for a list of possible values and <https://cloudfour.com/thinks/autofill-what-web-devs-should-know-but-dont/> for in-depth guidance on how to use autocomplete.
-}
withAutocomplete : String -> Format -> Format
withAutocomplete str format =
    { format | autocomplete = Just str }


{-| The type of `input` to use. The default is `text`.
-}
withInputType : String -> Format -> Format
withInputType str format =
    { format | inputType = Just str }


withValidation : (String -> Validation String) -> Format -> Format
withValidation validation format =
    { format | validation = validation }
