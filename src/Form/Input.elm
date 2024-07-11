module Form.Input exposing
    ( Input
    , baseInput, textInput, passwordInput, textArea, checkboxInput, radioInput
    , floatInput, floatSelectInput, intInput, intSelectInput, textSelectInput
    )

{-| Html input view helpers, wired for elm-form validation.

@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput

-}

import Html.Attributes as Attrs
import Form exposing (FieldState, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (FieldValue(..))
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (..)
import Json.Schema.Form.Options exposing (Options)
import Json.Decode as Json


{-| An input renders Html from a field state and list of additional attributes.
All input functions using this type alias are pre-wired with event handlers.
-}
type alias Input =
    FieldState -> List (Attribute Msg) -> Html Msg


{-| Untyped input
-}
baseInput : Options -> String -> (String -> FieldValue) -> InputType -> Input
baseInput options type__ toFieldValue inputType state attrs =
    let
        formAttrs =
            [ id state.path
            , type_ type__
            , value (Field.valueAsString state.value)
            , onInput (toFieldValue >> Input state.path inputType)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , Attrs.map never <|
                options.theme.txt
                    { withError = state.error /= Nothing
                    }
            ]
    in
    input (formAttrs ++ attrs) []

textArea : Options -> Input
textArea options state attrs =
    let
        formAttrs =
            [ id state.path
            , value (Field.valueAsString state.value)
            , onInput (String >> Input state.path Textarea)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , attribute "rows" "4"
            , Attrs.map never <|
                options.theme.txt
                    { withError = state.error /= Nothing
                    }
            ]
    in
    Html.textarea (formAttrs ++ attrs) []

fromIntInput : String -> FieldValue
fromIntInput s =
    if String.isEmpty s then
        Field.Empty

    else
        Maybe.withDefault (String s) <| Maybe.map Int <| String.toInt s


fromFloatInput : String -> FieldValue
fromFloatInput s =
    if String.isEmpty s then
        Field.Empty

    else
        Maybe.withDefault (String s) <| Maybe.map Number <| String.toFloat s


fromStringInput : String -> FieldValue
fromStringInput s =
    if String.isEmpty s then
        Field.Empty

    else
        Field.String s


{-| Text input.
-}
textInput : Options -> Input
textInput options =
    baseInput options "text" fromStringInput Text


{-| Text input.
-}
intInput : Options -> Input
intInput options state attrs =
    baseInput options "text" fromIntInput Text state ([attribute "type" "number"] ++ attrs)


{-| Text input.
-}
floatInput : Options -> Input
floatInput options state attrs =
    baseInput options "text" fromFloatInput Text state ([attribute "type" "number"] ++ attrs)


{-| Password input.
-}
passwordInput : Options -> Input
passwordInput options =
    baseInput options "password" fromStringInput Text


{-| Select input.
-}
baseSelectInput : List ( String, String ) -> (String -> FieldValue) -> Input
baseSelectInput options toFieldValue state attrs =
    let
        formAttrs =
            [ on
                "change"
                (targetValue |> Json.map (toFieldValue >> Input state.path Select))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]

        buildOption ( k, v ) =
            option [ value k, selected (Field.valueAsString state.value == k) ] [ text v ]
    in
    select (formAttrs ++ attrs) (List.map buildOption options)


{-| Text input.
-}
textSelectInput : List ( String, String ) -> Input
textSelectInput options =
    baseSelectInput options fromStringInput


{-| Text input.
-}
intSelectInput : List ( String, String ) -> Input
intSelectInput options =
    baseSelectInput options fromIntInput


{-| Text input.
-}
floatSelectInput : List ( String, String ) -> Input
floatSelectInput options =
    baseSelectInput options fromFloatInput


{-| Checkbox input.
-}
checkboxInput : Input
checkboxInput state attrs =
    let
        formAttrs =
            [ type_ "checkbox"
            , checked (Field.valueAsBool state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.path Checkbox)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    input (formAttrs ++ attrs) []


{-| Radio input.
-}
radioInput : String -> Input
radioInput value state attrs =
    let
        formAttrs =
            [ type_ "radio"
            , name state.path
            , HtmlAttr.value value
            , checked (Field.valueAsString state.value == value)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , on
                "change"
                (targetValue |> Json.map (String >> Input state.path Radio))
            ]
    in
    input (formAttrs ++ attrs) []
