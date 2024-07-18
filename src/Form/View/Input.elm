module Form.View.Input exposing
    ( Input
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
    , toggleInput
    )

import Form.FieldValue as FieldValue exposing (FieldValue(..))
import Form.Settings exposing (Settings)
import Form.State exposing (FieldState, Msg(..))
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Attributes.Extra as Attrs
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions as Schema
import UiSchema.Internal exposing (DefOptions)


type alias Input =
    FieldState -> Html Msg


baseTextInput : Settings -> DefOptions -> (String -> FieldValue) -> String -> Maybe Int -> Input
baseTextInput settings options toFieldValue inputType maxLength state =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , type_ inputType
            , value (FieldValue.asString state.value)
            , onInput (toFieldValue >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , case ( options.restrict, maxLength ) of
                ( True, Just n ) ->
                    Attrs.maxlength n

                _ ->
                    Attrs.empty
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                settings.theme.textInput
                    { trim = options.trim
                    , invalid = state.error /= Nothing
                    }
            ]
    in
    input formAttrs []


slider : Settings -> DefOptions -> Schema.SubSchema -> (String -> FieldValue) -> Input
slider settings options schema toFieldValue state =
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
                settings.theme.sliderInput
                    { trim = options.trim
                    }
            ]
    in
    div
        []
        [ div [ Attrs.style "display" "flex", Attrs.class "text-sm" ]
            [ span [ Attrs.style "flex-grow" "1", Attrs.class "text-left" ] [ text (String.fromFloat minLimit) ]
            , span [ Attrs.style "flex-grow" "1", Attrs.class "text-right" ] [ text (String.fromFloat maxLimit) ]
            ]
        , input formAttrs []
        ]


textArea : Settings -> DefOptions -> Maybe Int -> Input
textArea settings options maxLength state =
    let
        formAttrs =
            [ id <| Pointer.toString state.pointer
            , value (FieldValue.asString state.value)
            , onInput (String >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , attribute "rows" "4"
            , Attrs.disabled state.disabled
            , case ( options.restrict, maxLength ) of
                ( True, Just n ) ->
                    Attrs.maxlength n

                _ ->
                    Attrs.empty
            , Attrs.map never <|
                settings.theme.textArea
                    { trim = options.trim
                    , invalid = state.error /= Nothing
                    }
            ]
    in
    Html.textarea formAttrs []


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


textInput : Settings -> DefOptions -> String -> Maybe Int -> Input
textInput settings options inputType maxLength =
    baseTextInput settings options fromStringInput inputType maxLength


intInput : Settings -> DefOptions -> Input
intInput settings options state =
    baseTextInput settings options fromIntInput "number" Nothing state


floatInput : Settings -> DefOptions -> Input
floatInput settings options state =
    baseTextInput settings options fromFloatInput "number" Nothing state


intSlider : Settings -> DefOptions -> Schema.SubSchema -> Input
intSlider settings options schema =
    slider settings options schema fromIntInput


numberSlider : Settings -> DefOptions -> Schema.SubSchema -> Input
numberSlider settings options schema =
    slider settings options schema fromFloatInput


baseSelectInput : Settings -> List ( String, String ) -> (String -> FieldValue) -> Input
baseSelectInput settings valueList toFieldValue state =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , on
                "change"
                (targetValue |> Decode.map (toFieldValue >> Input state.pointer))
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            , Attrs.map never <|
                settings.theme.selectInput
                    { trim = False
                    , invalid = state.error /= Nothing
                    }
            ]

        buildOption ( k, v ) =
            option [ value k, selected (FieldValue.asString state.value == k) ] [ text v ]
    in
    select formAttrs (List.map buildOption valueList)


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


toggleInput : Settings -> Input
toggleInput settings state =
    let
        checked =
            if state.value == Bool True then
                True

            else
                False

        formAttrs =
            [ type_ "button"
            , Attrs.id (inputElementId state.formId state.pointer)
            , Attrs.map never <| settings.theme.toggleInput { checked = checked }
            , onClick (Input state.pointer (Bool <| not checked))
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            ]
    in
    button formAttrs
        [ span
            [ Attrs.map never <| settings.theme.toggleKnob { checked = checked }
            ]
            []
        ]


checkboxInput : Settings -> Input
checkboxInput settings state =
    let
        formAttrs =
            [ type_ "checkbox"
            , Attrs.id (inputElementId state.formId state.pointer)
            , Attrs.map never <| settings.theme.checkboxInput { invalid = state.error /= Nothing }
            , checked (FieldValue.asBool state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.pointer)
            , onFocus (Focus state.pointer)
            , onBlur Blur
            , Attrs.disabled state.disabled
            ]
    in
    input formAttrs []
