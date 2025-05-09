module Form.Widget.View exposing (viewWidget, errorString)

import Form.Error exposing (ErrorValue(..))
import Form.FieldValue exposing (FieldFormat(..), FieldType(..))
import Form.State exposing (Msg)
import Form.Widget exposing (..)
import Html exposing (Html, div)
import Html.Attributes as Attrs exposing (class)
import Html.Attributes.Extra as Attrs
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe


viewWidget : Widget -> Html Msg
viewWidget widget =
    case widget of
        WHorizontalLayout widgets ->
            div
                [ class ("grid gap-3 grid-cols-" ++ String.fromInt (List.length widgets))
                ]
                (List.map viewWidget widgets)

        WVerticalLayout widgets ->
            div [] (List.map viewWidget widgets)

        WGroup group ->
            div [ class "p-3 my-3 border border-gray-300" ] (div [ class "font-bold" ] [ Html.viewMaybe Html.text group.label ] :: List.map viewWidget group.elements)

        WCategorization categorization ->
            let
                menu =
                    div [ class "my-4 border-b" ] (List.map menuItem categorization.buttons)

                menuItem button =
                    Html.button
                        [ class "p-4 pb-2"
                        , Attrs.classList
                            [ ( "text-blue-500 border-b-2 border-blue-500", button.focus )
                            ]
                        , Events.onClick button.onClick
                        ]
                        [ Html.text button.label ]

                body =
                    div [] (List.map viewWidget categorization.elements)
            in
            div [] [ menu, body ]

        WLabel label ->
            div [ class "font-bold mt-4" ] [ Html.text label ]

        WControl options control ->
            viewControl options control


viewControl : Options -> Control -> Html Msg
viewControl options control =
    controlWrapper options <|
        case control of
            CCheckbox checkbox ->
                viewCheckbox options checkbox

            CTextInput textInput ->
                viewTextInput options textInput

            CTextArea textArea ->
                viewTextArea options textArea

            CSlider sliderInput ->
                viewSlider options sliderInput

            CRadioGroup radioGroup ->
                viewRadioGroup options radioGroup

            CSelect select ->
                viewSelect options select


controlWrapper : Options -> List (Html Msg) -> Html Msg
controlWrapper options =
    div [ class "my-4", Attrs.classList [ ( "opacity-50", options.disabled ) ] ]


viewCheckbox : Options -> Checkbox -> List (Html Msg)
viewCheckbox options checkbox =
    [ Html.label
        [ Attrs.for options.id
        , class "flex items-center space-x-4"
        ]
        [ Html.input
            [ Attrs.type_ "checkbox"
            , Attrs.id options.id
            , Attrs.classList [ ( "border-red-600", isInvalid options.validation ) ]
            , Attrs.checked checkbox.value
            , Attrs.disabled options.disabled
            , Events.onCheck checkbox.onCheck
            , Events.onFocus options.onFocus
            ]
            []
        , viewLabel options.label options.required
        ]

    -- TODO: show description as tooltip
    , viewErrorMessage options.validation
    ]


viewTextInput : Options -> TextInput -> List (Html Msg)
viewTextInput options textInput =
    [ viewLabel options.label options.required
    , Html.input
        [ Attrs.type_ <| inputType textInput.fieldType
        , Attrs.id options.id
        , Attrs.classList
            [ ( "border-red-600", isInvalid options.validation )
            , ( "w-full", not options.trim )
            ]
        , Attrs.value textInput.value
        , Attrs.disabled options.disabled
        , Events.onInput textInput.onInput
        , Events.onFocus options.onFocus
        , Maybe.unwrap Attrs.empty Attrs.maxlength textInput.restrict
        ]
        []
    , viewDescription options.description
    , viewErrorMessage options.validation
    ]


viewTextArea : Options -> TextArea -> List (Html Msg)
viewTextArea options textArea =
    [ viewLabel options.label options.required
    , Html.textarea
        [ Attrs.id options.id
        , Attrs.classList
            [ ( "border-red-600", isInvalid options.validation )
            , ( "w-full", not options.trim )
            ]
        , Attrs.value textArea.value
        , Attrs.disabled options.disabled
        , Events.onInput textArea.onInput
        , Events.onFocus options.onFocus
        , Attrs.attribute "rows" "4"
        , Maybe.unwrap Attrs.empty Attrs.maxlength textArea.restrict
        ]
        []
    , viewDescription options.description
    , viewErrorMessage options.validation
    ]


viewSlider : Options -> Slider -> List (Html Msg)
viewSlider options sliderInput =
    [ viewLabel options.label options.required
    , div [ Attrs.style "display" "flex", Attrs.class "text-sm" ]
        [ Html.span [ Attrs.style "flex-grow" "1", Attrs.class "text-left" ] [ Html.text sliderInput.min ]
        , Html.span [ Attrs.style "flex-grow" "1", Attrs.class "text-right" ] [ Html.text sliderInput.max ]
        ]
    , Html.input
        [ Attrs.id options.id
        , Attrs.classList
            [ ( "border-red-600", isInvalid options.validation )
            , ( "w-full", not options.trim )
            , ( "w-52", options.trim )
            ]
        , Attrs.value sliderInput.value
        , Attrs.disabled options.disabled
        , Events.onInput sliderInput.onInput
        , Events.onFocus options.onFocus
        , Attrs.attribute "rows" "4"
        , Attrs.type_ "range"
        , Attrs.attribute "min" sliderInput.min
        , Attrs.attribute "max" sliderInput.max
        , Attrs.attribute "step" sliderInput.step
        ]
        []
    , viewDescription options.description
    , viewErrorMessage options.validation
    ]


viewRadioGroup : Options -> RadioGroup -> List (Html Msg)
viewRadioGroup options radioGroup =
    let
        radio { id, label, onClick } =
            Html.label
                [ Attrs.for id
                , Attrs.classList
                    [ ( "mr-5 items-center", True )
                    , ( "flex", radioGroup.vertical )
                    ]
                ]
                [ Html.input
                    [ Attrs.type_ "radio"
                    , Attrs.id id
                    , Attrs.class "mr-3"
                    , Attrs.classList [ ( "border-red-600", isInvalid options.validation ) ]
                    , Attrs.checked <| label == radioGroup.value
                    , Attrs.disabled options.disabled
                    , Events.onClick onClick
                    , Events.onFocus options.onFocus
                    ]
                    []
                , viewLabel (Just label) options.required
                ]
    in
    [ viewLabel options.label options.required
    , div [] <|
        List.map radio radioGroup.valueList
    , viewDescription options.description
    , viewErrorMessage options.validation
    ]


viewSelect : Options -> Select -> List (Html Msg)
viewSelect options select =
    let
        buildOption v =
            Html.option [ Attrs.value v, Attrs.selected (select.value == v) ] [ Html.text v ]
    in
    [ viewLabel options.label options.required
    , Html.select
        [ Attrs.id options.id
        , Attrs.classList
            [ ( "border-red-600", isInvalid options.validation )
            , ( "w-full", not options.trim )
            ]
        , Attrs.disabled options.disabled
        , Events.on "change" (Events.targetValue |> Decode.map select.onChange)
        , Events.onFocus options.onFocus
        , Attrs.attribute "rows" "4"
        ]
        (List.map buildOption select.valueList)
    , viewDescription options.description
    , viewErrorMessage options.validation
    ]


inputType : FieldType -> String
inputType fieldType =
    case fieldType of
        StringField format ->
            case format of
                Text ->
                    "text"

                Email ->
                    "email"

                Date ->
                    "date"

                Time ->
                    "time"

                DateTime ->
                    "datetime-local"

        NumberField ->
            "number"

        IntField ->
            "number"


viewLabel : Maybe String -> Bool -> Html Msg
viewLabel label required =
    Html.viewMaybe
        (\l ->
            Html.span
                [ class "block text-sm my-1"
                ]
                [ Html.text <|
                    l
                        ++ (if required then
                                " *"

                            else
                                ""
                           )
                ]
        )
        label


viewDescription : Maybe String -> Html Msg
viewDescription description =
    Html.viewMaybe (\d -> Html.div [ class "text-slate-500 text-sm" ] [ Html.text d ]) description


viewErrorMessage : Validation -> Html Msg
viewErrorMessage validation =
    case validation of
        Invalid e ->
            Html.div [ class "text-red-600 text-sm" ] [ Html.text (errorString e) ]

        _ ->
            Html.nothing


errorString : ErrorValue -> String
errorString error =
    case error of
        Empty ->
            "is a required property"

        NotConst v ->
            case Encode.encode 0 v of
                "true" ->
                    "must be checked"

                "false" ->
                    "must be unchecked"

                s ->
                    "must be equal to " ++ s

        InvalidString ->
            "not a valid string"

        InvalidFormat _ ->
            "not the correct format"

        InvalidInt ->
            "not a valid integer"

        InvalidFloat ->
            "not a valid number"

        InvalidBool ->
            "not a valid option"

        InvalidNull ->
            "not a null"

        LessIntThan n ->
            "can not be smaller than " ++ String.fromInt n

        LessEqualIntThan n ->
            "can not be smaller or equal than " ++ String.fromInt n

        GreaterIntThan n ->
            "can not be greater than " ++ String.fromInt n

        GreaterEqualIntThan n ->
            "can not be greater or equal than " ++ String.fromInt n

        LessFloatThan n ->
            "can not be smaller than " ++ String.fromFloat n

        LessEqualFloatThan n ->
            "can not be smaller or equal than " ++ String.fromFloat n

        GreaterFloatThan n ->
            "can not be greater than " ++ String.fromFloat n

        GreaterEqualFloatThan n ->
            "can not be greater or equal than " ++ String.fromFloat n

        ShorterStringThan n ->
            "must NOT have fewer than " ++ String.fromInt n ++ " characters"

        LongerStringThan n ->
            "must NOT have more than " ++ String.fromInt n ++ " characters"

        NotMultipleOfInt n ->
            "must be a multiple of " ++ String.fromInt n ++ "."

        NotMultipleOfFloat n ->
            "must be a multiple of " ++ String.fromFloat n ++ "."

        NotIncludedIn _ ->
            "is not a valid selection from the list."

        Unimplemented s ->
            "unimplemented: " ++ s
