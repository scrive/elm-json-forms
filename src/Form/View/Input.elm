module Form.View.Input exposing
    ( Input
    , InputType(..)
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

import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Html.Attributes.Extra as Attrs
import Form.Settings exposing (Settings)
import Form.State exposing (FieldState, Msg(..))
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import UiSchema.Internal exposing (Options)
import Json.Decode as Decode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions as Schema


type alias Input =
    FieldState -> List (Attribute Msg) -> Html Msg


type InputType
    = Text


baseTextInput : Settings -> Options -> (String -> FieldValue) -> InputType -> Input
baseTextInput settings options toFieldValue inputType state attrs =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , type_ "text"
            , value (FieldValue.asString state.value)
            , onInput (toFieldValue >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                settings.theme.textInput
                    { trim = options.trim == Just True
                    , invalid = state.error /= Nothing
                    }
            ]
    in
    input (formAttrs ++ attrs) []


slider : Settings -> Options -> Schema.SubSchema -> (String -> FieldValue) -> InputType -> Input
slider settings options schema toFieldValue inputType state attrs =
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
            , onInput (toFieldValue >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.attribute "min" (String.fromFloat minLimit)
            , Attrs.attribute "max" (String.fromFloat maxLimit)
            , Attrs.attribute "step" (String.fromFloat step)
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                settings.theme.sliderInput { trim = Maybe.withDefault False options.trim }
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


textArea : Settings -> Options -> Input
textArea settings options state attrs =
    let
        formAttrs =
            [ id <| Pointer.toString state.pointer
            , value (FieldValue.asString state.value)
            , onInput (String >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , attribute "rows" "4"
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                settings.theme.textArea
                    { trim = Maybe.withDefault False options.trim
                    , invalid = state.error /= Nothing
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


textInput : Settings -> Options -> Input
textInput settings options =
    baseTextInput settings options fromStringInput Text


intInput : Settings -> Options -> Input
intInput settings options state attrs =
    baseTextInput settings options fromIntInput Text state (attribute "type" "number" :: attrs)


floatInput : Settings -> Options -> Input
floatInput settings options state attrs =
    baseTextInput settings options fromFloatInput Text state (attribute "type" "number" :: attrs)


intSlider : Settings -> Options -> Schema.SubSchema -> Input
intSlider settings options schema =
    slider settings options schema fromIntInput Text


numberSlider : Settings -> Options -> Schema.SubSchema -> Input
numberSlider settings options schema =
    slider settings options schema fromFloatInput Text


baseSelectInput : Settings -> List ( String, String ) -> (String -> FieldValue) -> Input
baseSelectInput settings valueList toFieldValue state attrs =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , on
                "change"
                (targetValue |> Decode.map (toFieldValue >> Input state.pointer))
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            , Attrs.map never <| settings.theme.selectInput { trim = False, invalid = state.error /= Nothing }
            ]

        buildOption ( k, v ) =
            option [ value k, selected (FieldValue.asString state.value == k) ] [ text v ]
    in
    select (formAttrs ++ attrs) (List.map buildOption valueList)


textSelectInput : Settings -> List ( String, String ) -> Input
textSelectInput settings valueList =
    baseSelectInput settings valueList fromStringInput


intSelectInput : Settings -> List ( String, String ) -> Input
intSelectInput settings valueList =
    baseSelectInput settings valueList fromIntInput


floatSelectInput : Settings -> List ( String, String ) -> Input
floatSelectInput settings valueList =
    baseSelectInput settings valueList fromFloatInput


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
            , onCheck (Bool >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            ]
    in
    input (formAttrs ++ attrs) []
