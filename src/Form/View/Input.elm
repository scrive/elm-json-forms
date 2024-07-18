module Form.View.Input exposing
    ( Input
    , baseSelectInput
    , baseTextInput
    , checkboxInput
    , inputElementGroupId
    , inputElementId
    , slider
    , textArea
    , toggleInput
    )

import Form.FieldValue as FieldValue exposing (FieldType(..), FieldValue(..))
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


baseTextInput : Settings -> DefOptions -> FieldType -> String -> Maybe Int -> Input
baseTextInput settings options fieldType inputType maxLength state =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , type_ inputType
            , value (FieldValue.asString state.value)
            , onInput (FieldValue.fromFieldInput fieldType >> Input state.pointer)
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


slider : Settings -> DefOptions -> Schema.SubSchema -> FieldType -> Input
slider settings options schema fieldType state =
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
            , onInput (FieldValue.fromFieldInput fieldType >> Input state.pointer)
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


baseSelectInput : FieldType -> Settings -> List ( String, String ) -> Input
baseSelectInput fieldType settings valueList state =
    let
        formAttrs =
            [ id (inputElementId state.formId state.pointer)
            , on
                "change"
                (targetValue |> Decode.map (FieldValue.fromFieldInput fieldType >> Input state.pointer))
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
