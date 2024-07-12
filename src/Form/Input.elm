module Form.Input exposing
    ( Input
    , baseInput
    , checkboxInput
    , floatInput
    , floatSelectInput
    , inputElementGroupId
    , inputElementId
    , intInput
    , intSelectInput
    , intSlider
    , numberSlider
    , textArea
    , textInput
    , textSelectInput
    )

import Form exposing (FieldState, InputType(..), Msg(..))
import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions as Schema
import Json.Schema.Form.Options exposing (Options)


type alias Input =
    FieldState -> List (Attribute Msg) -> Html Msg


baseInput : Options -> (String -> FieldValue) -> InputType -> Input
baseInput options toFieldValue inputType state attrs =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , type_ "text"
            , value (FieldValue.asString state.value)
            , onInput (toFieldValue >> Input state.pointer inputType)
            , onFocus (Focus state.pointer)
            , onBlur (Blur state.pointer)
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                options.theme.fieldInput
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
            [ id <| Pointer.toString state.pointer
            , type_ "range"
            , value (FieldValue.asString state.value)
            , onInput (toFieldValue >> Input state.pointer inputType)
            , onFocus (Focus state.pointer)
            , onBlur (Blur state.pointer)
            , Attrs.attribute "min" (String.fromFloat minLimit)
            , Attrs.attribute "max" (String.fromFloat maxLimit)
            , Attrs.attribute "step" (String.fromFloat step)
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                options.theme.fieldInput
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
            [ id <| Pointer.toString state.pointer
            , value (FieldValue.asString state.value)
            , onInput (String >> Input state.pointer Textarea)
            , onFocus (Focus state.pointer)
            , onBlur (Blur state.pointer)
            , attribute "rows" "4"
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                options.theme.fieldInput
                    { withError = state.error /= Nothing
                    }
            ]
    in
    Html.textarea (formAttrs ++ attrs) []


fromIntInput : String -> FieldValue
fromIntInput s =
    if String.isEmpty s then
        FieldValue.Empty

    else
        Maybe.withDefault (String s) <| Maybe.map Int <| String.toInt s


fromFloatInput : String -> FieldValue
fromFloatInput s =
    if String.isEmpty s then
        FieldValue.Empty

    else
        Maybe.withDefault (String s) <| Maybe.map Number <| String.toFloat s


fromStringInput : String -> FieldValue
fromStringInput s =
    if String.isEmpty s then
        FieldValue.Empty

    else
        FieldValue.String s


textInput : Options -> Input
textInput options =
    baseInput options fromStringInput Text


intInput : Options -> Input
intInput options state attrs =
    baseInput options fromIntInput Text state (attribute "type" "number" :: attrs)


floatInput : Options -> Input
floatInput options state attrs =
    baseInput options fromFloatInput Text state (attribute "type" "number" :: attrs)


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
            [ id (inputElementId state.formId state.pointer)
            , on
                "change"
                (targetValue |> Decode.map (toFieldValue >> Input state.pointer Select))
            , onFocus (Focus state.pointer)
            , onBlur (Blur state.pointer)
            , Attrs.disabled state.disabled
            , Attrs.map never <| options.theme.fieldInput { withError = state.error /= Nothing }
            ]

        buildOption ( k, v ) =
            option [ value k, selected (FieldValue.asString state.value == k) ] [ text v ]
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


inputElementId : String -> Pointer -> String
inputElementId formId pointer =
    formId ++ "-" ++ Pointer.toString pointer ++ "-input"


inputElementGroupId : String -> String -> String
inputElementGroupId formId path =
    formId ++ "-" ++ path


checkboxInput : Input
checkboxInput state attrs =
    let
        formAttrs =
            [ type_ "checkbox"
            , checked (FieldValue.asBool state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.pointer Checkbox)
            , onFocus (Focus state.pointer)
            , onBlur (Blur state.pointer)
            , Attrs.disabled state.disabled
            ]
    in
    input (formAttrs ++ attrs) []
