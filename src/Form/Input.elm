module Form.Input exposing
    ( Input
    , baseInput, textInput, passwordInput, textArea, checkboxInput, radioInput
    , floatInput, floatSelectInput, intInput, intSelectInput, textSelectInput
    )

{-| Html input view helpers, wired for elm-form validation.

@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput

-}

import Form exposing (FieldState, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (FieldValue(..))
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


{-| An input renders Html from a field state and list of additional attributes.
All input functions using this type alias are pre-wired with event handlers.
-}
type alias Input e =
    FieldState e -> List (Attribute Msg) -> Html Msg


{-| Untyped input, first param is `type` attribute.
-}
baseInput : String -> (String -> FieldValue) -> InputType -> Input e
baseInput t toFieldValue inputType state attrs =
    let
        formAttrs =
            [ type_ t
            , value (Maybe.andThen Field.valueAsString state.value |> Maybe.withDefault "")
            , onInput (toFieldValue >> Input state.path inputType)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    input (formAttrs ++ attrs) []


{-| Text input.
-}
textInput : Input e
textInput =
    baseInput "text" String Text


{-| Text input.
-}
intInput : Input e
intInput =
    baseInput "text" (\x -> Maybe.withDefault (String x) <| Maybe.map Int <| String.toInt x) Text


{-| Text input.
-}
floatInput : Input e
floatInput =
    baseInput "text" (\x -> Maybe.withDefault (String x) <| Maybe.map Number <| String.toFloat x) Text


{-| Password input.
-}
passwordInput : Input e
passwordInput =
    baseInput "password" String Text


{-| Textarea.
-}
textArea : Input e
textArea state attrs =
    let
        formAttrs =
            [ value (state.value |> Maybe.andThen Field.valueAsString |> Maybe.withDefault "")
            , onInput (String >> Input state.path Textarea)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    Html.textarea (formAttrs ++ attrs) []


{-| Select input.
-}
baseSelectInput : List ( String, String ) -> (String -> FieldValue) -> Input e
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
            option [ value k, selected (Maybe.andThen Field.valueAsString state.value == Just k) ] [ text v ]
    in
    select (formAttrs ++ attrs) (List.map buildOption options)


{-| Text input.
-}
textSelectInput : List ( String, String ) -> Input e
textSelectInput options =
    baseSelectInput options String


{-| Text input.
-}
intSelectInput : List ( String, String ) -> Input e
intSelectInput options =
    baseSelectInput options (\x -> Maybe.withDefault (String x) <| Maybe.map Int <| String.toInt x)


{-| Text input.
-}
floatSelectInput : List ( String, String ) -> Input e
floatSelectInput options =
    baseSelectInput options (\x -> Maybe.withDefault (String x) <| Maybe.map Number <| String.toFloat x)


{-| Checkbox input.
-}
checkboxInput : Input e
checkboxInput state attrs =
    let
        formAttrs =
            [ type_ "checkbox"
            , checked (Maybe.andThen Field.valueAsBool state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.path Checkbox)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    input (formAttrs ++ attrs) []


{-| Radio input.
-}
radioInput : String -> Input e
radioInput value state attrs =
    let
        formAttrs =
            [ type_ "radio"
            , name state.path
            , HtmlAttr.value value
            , checked (Maybe.andThen Field.valueAsString state.value == Just value)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , on
                "change"
                (targetValue |> Json.map (String >> Input state.path Radio))
            ]
    in
    input (formAttrs ++ attrs) []
