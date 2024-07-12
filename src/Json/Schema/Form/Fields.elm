module Json.Schema.Form.Fields exposing (TextFieldType(..), uiSchemaView)

import Dict
import Form.State as F exposing (FormState)
import Form.Error exposing (ErrorValue)
import Form.Input as Input
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes as Attrs
    exposing
        ( class
        , for
        , id
        , style
        , type_
        )
import Html.Events exposing (onClick)
import Html.Extra as Html
import Json.Decode as Decode exposing (Value)
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions
    exposing
        ( Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        )
import Json.Schema.Form.Options exposing (Options)
import Json.Schema.Form.Theme exposing (Theme)
import Json.Schema.Form.UiSchema as UI exposing (Effect(..), UiSchema)
import Json.Schema.Form.Validation exposing (validation)
import List.Extra as List
import Maybe.Extra as Maybe
import Validation


type alias UiState =
    { disabled : Bool
    , uiPath : List Int
    }


type AppliedEffect
    = Hidden
    | Disabled


appendPathElement : Int -> UiState -> UiState
appendPathElement i st =
    { st | uiPath = List.append st.uiPath [ i ] }


uiSchemaView : Options -> UiState -> UiSchema -> Schema -> FormState -> List (Html F.Msg)
uiSchemaView options uiState uiSchema schema form =
    let
        ruleEffect : Maybe AppliedEffect
        ruleEffect =
            computeRule form.value (UI.getRule uiSchema)

        newUiState =
            { uiState | disabled = ruleEffect == Just Disabled }
    in
    applyEffect ruleEffect <|
        case uiSchema of
            UI.UiControl c ->
                [ controlView options newUiState schema c form ]

            UI.UiHorizontalLayout hl ->
                horizontalLayoutView options newUiState schema form hl

            UI.UiVerticalLayout vl ->
                verticalLayoutView options newUiState schema form vl

            UI.UiGroup g ->
                groupView options newUiState schema form g

            UI.UiCategorization c ->
                categorizationView options newUiState schema form c

            UI.UiLabel l ->
                [ Html.div [ Attrs.map never options.theme.label ] [ text l.text ] ]


applyEffect : Maybe AppliedEffect -> List (Html F.Msg) -> List (Html F.Msg)
applyEffect effect x =
    case effect of
        Just Hidden ->
            []

        Just Disabled ->
            [ div [ Attrs.class "opacity-50" ] x ]

        Nothing ->
            x


computeRule : Value -> Maybe UI.Rule -> Maybe AppliedEffect
computeRule formValue mRule =
    let
        condition rule =
            case Pointer.pointedValue rule.condition.scope formValue of
                Nothing ->
                    False

                Just v ->
                    Validation.isOk <| validation rule.condition.schema v

        go rule =
            case ( rule.effect, condition rule ) of
                ( EffectDisable, True ) ->
                    Just Disabled

                ( EffectEnable, False ) ->
                    Just Disabled

                ( EffectShow, False ) ->
                    Just Hidden

                ( EffectHide, True ) ->
                    Just Hidden

                _ ->
                    Nothing
    in
    Maybe.andThen go mRule


horizontalLayoutView : Options -> UiState -> Schema -> FormState -> UI.HorizontalLayout -> List (Html F.Msg)
horizontalLayoutView options uiState wholeSchema form hl =
    [ div [ Attrs.map never options.theme.horizontalLayout ] <|
        List.indexedMap
            (\ix us ->
                div
                    [ Attrs.map never options.theme.horizontalLayoutItem ]
                    (uiSchemaView options (appendPathElement ix uiState) us wholeSchema form)
            )
            hl.elements
    ]


verticalLayoutView : Options -> UiState -> Schema -> FormState -> UI.VerticalLayout -> List (Html F.Msg)
verticalLayoutView options uiState wholeSchema form vl =
    List.indexedMap
        (\ix us ->
            div
                []
                (uiSchemaView options (appendPathElement ix uiState) us wholeSchema form)
        )
        vl.elements


groupView : Options -> UiState -> Schema -> FormState -> UI.Group -> List (Html F.Msg)
groupView options uiState wholeSchema form group =
    let
        title =
            Maybe.unwrap [] (\l -> [ Html.div [ Attrs.map never options.theme.groupLabel ] [ text l ] ]) group.label

        contents =
            verticalLayoutView options uiState wholeSchema form { elements = group.elements, rule = group.rule }
    in
    [ div [ Attrs.map never options.theme.group ] (title ++ contents)
    ]


categorizationView : Options -> UiState -> Schema -> FormState -> UI.Categorization -> List (Html F.Msg)
categorizationView options uiState wholeSchema form categorization =
    let
        focusedCategoryIx =
            Maybe.withDefault 0 <| Dict.get uiState.uiPath form.categoryFocus

        categoryButton ix category =
            if computeRule form.value category.rule == Just Hidden then
                Nothing

            else
                Just <|
                    button
                        [ Attrs.map never <| options.theme.categorizationMenuItem { focus = focusedCategoryIx == ix }
                        , onClick <| F.FocusCategory uiState.uiPath ix
                        ]
                        [ text category.label ]
    in
    div
        [ Attrs.map never options.theme.categorizationMenu
        ]
        (Maybe.values <| List.indexedMap categoryButton categorization.elements)
        :: Maybe.unwrap [] (categoryView options (appendPathElement focusedCategoryIx uiState) wholeSchema form) (List.getAt focusedCategoryIx categorization.elements)


categoryView : Options -> UiState -> Schema -> FormState -> UI.Category -> List (Html F.Msg)
categoryView options uiState wholeSchema form category =
    let
        ruleEffect =
            computeRule form.value category.rule

        newUiState =
            { uiState | disabled = ruleEffect == Just Disabled }
    in
    applyEffect ruleEffect <|
        verticalLayoutView options newUiState wholeSchema form { elements = category.elements, rule = category.rule }


controlView : Options -> UiState -> Schema -> UI.Control -> FormState -> Html F.Msg
controlView options uiState wholeSchema control form =
    let
        mControlSchema =
            UI.pointToSchema wholeSchema control.scope

        fieldState =
            F.getField uiState.disabled control.scope form

        controlBody schema_ =
            case schema_ of
                BooleanSchema _ ->
                    Html.nothing

                ObjectSchema schema ->
                    case schema.type_ of
                        SingleType IntegerType ->
                            if schema.enum /= Nothing then
                                select options control schema fieldState IntField

                            else if (control.options |> Maybe.andThen .slider) == Just True then
                                intSlider options control schema fieldState

                            else
                                intInput options control schema fieldState

                        SingleType NumberType ->
                            if schema.enum /= Nothing then
                                select options control schema fieldState NumberField

                            else if (control.options |> Maybe.andThen .slider) == Just True then
                                numberSlider options control schema fieldState

                            else
                                numberInput options control schema fieldState

                        SingleType StringType ->
                            if schema.enum /= Nothing then
                                select options control schema fieldState StringField

                            else if (control.options |> Maybe.andThen .multi) == Just True then
                                textarea options control schema fieldState

                            else
                                textInput options control schema fieldState

                        SingleType BooleanType ->
                            checkbox options control schema fieldState

                        _ ->
                            Html.nothing
    in
    Html.viewMaybe
        (\cs ->
            div
                [ id (Input.inputElementGroupId form.formId (Pointer.toString control.scope))
                , Attrs.map never options.theme.fieldGroup
                ]
                [ controlBody cs ]
        )
        mControlSchema


type TextFieldType
    = NumberField
    | IntField
    | StringField


textInput : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
textInput options control schema fieldState =
    let
        inputType : String
        inputType =
            case schema.format of
                Just "email" ->
                    "email"

                Just "idn-email" ->
                    "email"

                Just "date" ->
                    "date"

                Just "time" ->
                    "time"

                Just "date-time" ->
                    "datetime-local"

                Just "month" ->
                    "month"

                Just "week" ->
                    "week"

                Just "hostname" ->
                    "url"

                Just "idn-hostname" ->
                    "url"

                Just "uri" ->
                    "url"

                Just "iri" ->
                    "url"

                _ ->
                    "text"
    in
    fieldGroup (Input.textInput options fieldState [ type_ inputType ])
        options
        control
        schema
        fieldState


intInput : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
intInput options control schema fieldState =
    fieldGroup (Input.intInput options fieldState [])
        options
        control
        schema
        fieldState


numberInput : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
numberInput options control schema fieldState =
    fieldGroup (Input.floatInput options fieldState [])
        options
        control
        schema
        fieldState


intSlider : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
intSlider options control schema fieldState =
    fieldGroup (Input.intSlider options schema fieldState [])
        options
        control
        schema
        fieldState


numberSlider : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
numberSlider options control schema fieldState =
    fieldGroup (Input.numberSlider options schema fieldState [])
        options
        control
        schema
        fieldState


textarea : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
textarea options control schema state =
    fieldGroup (Input.textArea options state []) options control schema state


checkbox : Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
checkbox options control schema fieldState =
    let
        inputField : Html F.Msg
        inputField =
            div [ Attrs.map never options.theme.checkboxRow ]
                [ Input.checkboxInput fieldState
                    [ Attrs.map never <| options.theme.checkboxInput { withError = fieldState.error /= Nothing }
                    , id (Input.inputElementId fieldState.formId fieldState.pointer)
                    ]
                , Html.viewMaybe identity <| fieldLabel options.theme control.label schema control.scope
                ]

        description : Html F.Msg
        description =
            Html.viewMaybe identity <| fieldDescription options.theme schema

        errorMessage : Html F.Msg
        errorMessage =
            Html.viewMaybe identity <| error options.theme options.errors fieldState
    in
    label [ for (Input.inputElementId fieldState.formId fieldState.pointer) ]
        [ inputField
        , description
        , errorMessage
        ]


select : Options -> UI.Control -> SubSchema -> F.FieldState -> TextFieldType -> Html F.Msg
select options control schema fieldState fieldType =
    let
        values : List String
        values =
            Maybe.toList schema.enum |> List.concat |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "") |> List.append [ "" ]

        items : List ( String, String )
        items =
            List.map (\v -> ( v, v )) values

        inputType =
            case fieldType of
                StringField ->
                    Input.textSelectInput

                NumberField ->
                    Input.floatSelectInput

                IntField ->
                    Input.intSelectInput
    in
    fieldGroup
        (inputType options items fieldState [])
        options
        control
        schema
        fieldState


fieldGroup : Html F.Msg -> Options -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
fieldGroup inputField options control schema fieldState =
    let
        title : Maybe (Html F.Msg)
        title =
            fieldLabel options.theme control.label schema control.scope

        showDescription =
            Maybe.andThen .showUnfocusedDescription control.options == Just True || fieldState.hasFocus

        description : Html F.Msg
        description =
            Html.viewMaybe identity <|
                if showDescription then
                    fieldDescription options.theme schema

                else
                    Nothing

        errorMessage : Html F.Msg
        errorMessage =
            Html.viewMaybe identity <| error options.theme options.errors fieldState
    in
    label [ for (Input.inputElementId fieldState.formId fieldState.pointer) ]
        [ Html.viewMaybe identity title
        , inputField
        , description
        , errorMessage
        ]


fieldLabel : Theme -> Maybe UI.ControlLabel -> SubSchema -> Pointer -> Maybe (Html F.Msg)
fieldLabel theme label schema pointer =
    let
        fallback =
            schema.title
                -- If it does not have a title, derive from property name, unCamelCasing it
                |> Maybe.orElse (List.last pointer |> Maybe.map UI.fieldNameToTitle)
                |> Maybe.withDefault ""

        render str =
            span [ Attrs.map never theme.fieldLabel ] [ text str ]
    in
    case label of
        Just (UI.StringLabel s) ->
            Just <| render s

        Just (UI.BoolLabel False) ->
            Nothing

        Just (UI.BoolLabel True) ->
            Just <| render fallback

        Nothing ->
            Just <| render fallback


fieldDescription : Theme -> SubSchema -> Maybe (Html F.Msg)
fieldDescription theme schema =
    schema.description
        |> Maybe.map (\str -> div [ Attrs.map never theme.fieldDescription ] [ text str ])


error : Theme -> (Pointer -> ErrorValue -> String) -> F.FieldState -> Maybe (Html F.Msg)
error theme func f =
    f.error
        |> Maybe.map
            (\err ->
                div
                    [ Attrs.map never theme.fieldError
                    , style "display" "block"
                    ]
                    [ text (func f.pointer err) ]
            )
