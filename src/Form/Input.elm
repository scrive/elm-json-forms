module Form.Input exposing
    ( Input
    , baseInput, textInput, textArea, checkboxInput, radioInput
    , floatInput, floatSelectInput, intInput, intSelectInput, intSlider, numberSlider, textSelectInput
    )

{-| Html input view helpers, wired for elm-form validation.

@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput

-}

import Form exposing (FieldState, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (FieldValue(..))
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Json.Schema.Definitions as Schema
import Json.Schema.Form.Options exposing (Options)
import Maybe.Extra as Maybe


{-| An input renders Html from a field state and list of additional attributes.
All input functions using this type alias are pre-wired with event handlers.
-}
type alias Input =
    FieldState -> List (Attribute Msg) -> Html Msg


baseInput : Options -> String -> (String -> FieldValue) -> InputType -> Input
baseInput options type__ toFieldValue inputType state attrs =
    let
        formAttrs =
            [ id (state.path ++ "-input")
            , type_ type__
            , value (Field.valueAsString state.value)
            , onInput (toFieldValue >> Input state.path inputType)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , Attrs.map never options.theme.fieldInput
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                options.theme.txt
                    { withError = state.error /= Nothing
                    }
            ]
    in
    input (formAttrs ++ attrs) []


slider : Options -> Schema.SubSchema -> (String -> FieldValue) -> InputType -> Input
slider options schema toFieldValue inputType state attrs =
    let
        step =
            Maybe.withDefault 1.0 schema.multipleOf

        minimum =
            Maybe.withDefault 1.0 schema.minimum

        maximum =
            Maybe.withDefault 10.0 schema.maximum

        minLimit =
            case schema.exclusiveMinimum of
                Just (Schema.BoolBoundary False) ->
                    minimum

                Just (Schema.BoolBoundary True) ->
                    minimum + step

                Just (Schema.NumberBoundary x) ->
                    x + step

                _ ->
                    minimum

        maxLimit =
            case schema.exclusiveMaximum of
                Just (Schema.BoolBoundary True) ->
                    maximum - step

                Just (Schema.NumberBoundary x) ->
                    x - step

                _ ->
                    maximum

        formAttrs =
            [ id state.path
            , type_ "range"
            , value (Field.valueAsString state.value)
            , onInput (toFieldValue >> Input state.path inputType)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , Attrs.attribute "min" (String.fromFloat minLimit)
            , Attrs.attribute "max" (String.fromFloat maxLimit)
            , Attrs.attribute "step" (String.fromFloat step)
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                options.theme.txt
                    { withError = state.error /= Nothing
                    }
            ]
    in
    div
        []
        [ div [ Attrs.style "display" "flex", Attrs.class "text-sm" ]
            [ span [ Attrs.style "flex-grow" "1", Attrs.class "text-left" ] [ text (String.fromFloat minLimit) ]
            , span [ Attrs.style "flex-grow" "1", Attrs.class "text-right" ] [ text (String.fromFloat maxLimit) ]
            ]
        , input (formAttrs ++ attrs) []
        ]


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
            , Attrs.disabled state.disabled
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


textInput : Options -> Input
textInput options =
    baseInput options "text" fromStringInput Text


intInput : Options -> Input
intInput options state attrs =
    baseInput options "text" fromIntInput Text state ([ attribute "type" "number" ] ++ attrs)


floatInput : Options -> Input
floatInput options state attrs =
    baseInput options "text" fromFloatInput Text state ([ attribute "type" "number" ] ++ attrs)


passwordInput : Options -> Input
passwordInput options =
    baseInput options "password" fromStringInput Text


intSlider : Options -> Schema.SubSchema -> Input
intSlider options schema =
    slider options schema fromIntInput Text


numberSlider : Options -> Schema.SubSchema -> Input
numberSlider options schema =
    slider options schema fromFloatInput Text


baseSelectInput : Options -> List ( String, String ) -> (String -> FieldValue) -> Input
baseSelectInput options valueList toFieldValue state attrs =
    let
        formAttrs =
            [ id (state.path ++ "-input")
            , on
                "change"
                (targetValue |> Json.map (toFieldValue >> Input state.path Select))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , Attrs.disabled state.disabled
            , Attrs.map never <| options.theme.select { withError = state.error /= Nothing }
            ]

        buildOption ( k, v ) =
            option [ value k, selected (Field.valueAsString state.value == k) ] [ text v ]
    in
    select (formAttrs ++ attrs) (List.map buildOption valueList)


textSelectInput : Options -> List ( String, String ) -> Input
textSelectInput options valueList =
    baseSelectInput options valueList fromStringInput


intSelectInput : Options -> List ( String, String ) -> Input
intSelectInput options valueList =
    baseSelectInput options valueList fromIntInput


floatSelectInput : Options -> List ( String, String ) -> Input
floatSelectInput options valueList =
    baseSelectInput options valueList fromFloatInput


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
            , Attrs.disabled state.disabled
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
            , Attrs.value value
            , checked (Field.valueAsString state.value == value)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , Attrs.disabled state.disabled
            , on
                "change"
                (targetValue |> Json.map (String >> Input state.path Radio))
            ]
    in
    input (formAttrs ++ attrs) []
