module Form.View exposing (view)

import Dict
import Form.Error exposing (ErrorValue)
import Form.Options exposing (Options)
import Form.State as F exposing (Form, FormState)
import Form.Theme exposing (Theme)
import Form.View.Input as Input
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Html.Extra as Html
import Json.Decode as Decode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema(..), SingleType(..), SubSchema, Type(..))
import List.Extra as List
import Maybe.Extra as Maybe
import UiSchema.Internal as UI exposing (UiSchema)
import UiSchema.Rule as Rule


type alias UiState =
    { disabled : Bool
    , uiPath : List Int
    , uiSchema : UiSchema
    }


walkState : Int -> UiSchema -> UiState -> UiState
walkState i uiSchema st =
    { st | uiPath = List.append st.uiPath [ i ], uiSchema = uiSchema }


view : Form -> UiState -> List (Html F.Msg)
view form uiState =
    let
        ruleEffect : Maybe Rule.AppliedEffect
        ruleEffect =
            Rule.computeRule form.state.value (UI.getRule uiState.uiSchema)

        newUiState =
            { uiState | disabled = ruleEffect == Just Rule.Disabled }
    in
    applyEffect form.options.theme ruleEffect <|
        case uiState.uiSchema of
            UI.UiControl c ->
                [ controlView form.options newUiState form.schema c form.state ]

            UI.UiHorizontalLayout hl ->
                horizontalLayoutView form newUiState hl

            UI.UiVerticalLayout vl ->
                verticalLayoutView form newUiState vl

            UI.UiGroup g ->
                groupView form newUiState g

            UI.UiCategorization c ->
                categorizationView form newUiState c

            UI.UiLabel l ->
                [ Html.div [ Attrs.map never form.options.theme.label ] [ text l.text ] ]


applyEffect : Theme -> Maybe Rule.AppliedEffect -> List (Html F.Msg) -> List (Html F.Msg)
applyEffect theme effect x =
    case effect of
        Just Rule.Hidden ->
            []

        Just Rule.Disabled ->
            [ div [ Attrs.map never theme.disabledElems ] x ]

        Nothing ->
            x


horizontalLayoutView : Form -> UiState -> UI.HorizontalLayout -> List (Html F.Msg)
horizontalLayoutView form uiState hl =
    [ div [ Attrs.map never form.options.theme.horizontalLayout ] <|
        List.indexedMap
            (\ix us ->
                div
                    [ Attrs.map never form.options.theme.horizontalLayoutItem ]
                    (view form (walkState ix us uiState))
            )
            hl.elements
    ]


verticalLayoutView : Form -> UiState -> UI.VerticalLayout -> List (Html F.Msg)
verticalLayoutView form uiState vl =
    List.indexedMap
        (\ix us ->
            div
                []
                (view form (walkState ix us uiState))
        )
        vl.elements


groupView : Form -> UiState -> UI.Group -> List (Html F.Msg)
groupView form uiState group =
    let
        title =
            Maybe.unwrap [] (\l -> [ Html.div [ Attrs.map never form.options.theme.groupLabel ] [ text l ] ]) group.label

        contents =
            verticalLayoutView form uiState { elements = group.elements, rule = group.rule }
    in
    [ div [ Attrs.map never form.options.theme.group ] (title ++ contents)
    ]


categorizationView : Form -> UiState -> UI.Categorization -> List (Html F.Msg)
categorizationView form uiState categorization =
    let
        focusedCategoryIx =
            Maybe.withDefault 0 <| Dict.get uiState.uiPath form.state.categoryFocus

        categoryButton ix category =
            if Rule.computeRule form.state.value category.rule == Just Rule.Hidden then
                Nothing

            else
                Just <|
                    button
                        [ Attrs.map never <| form.options.theme.categorizationMenuItem { focus = focusedCategoryIx == ix }
                        , onClick <| F.FocusCategory uiState.uiPath ix
                        ]
                        [ text category.label ]

        categoryUiState cat =
            walkState focusedCategoryIx (UI.UiVerticalLayout { elements = cat.elements, rule = cat.rule }) uiState
    in
    div
        [ Attrs.map never form.options.theme.categorizationMenu
        ]
        (Maybe.values <| List.indexedMap categoryButton categorization.elements)
        :: Maybe.unwrap [] (\cat -> view form (categoryUiState cat)) (List.getAt focusedCategoryIx categorization.elements)


controlView : Options -> UiState -> Schema -> UI.Control -> FormState -> Html F.Msg
controlView options uiState wholeSchema control form =
    let
        mControlSchema =
            UI.pointToSchema wholeSchema control.scope

        fieldState =
            F.fieldState uiState.disabled control.scope form

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
                [ Attrs.id (Input.inputElementGroupId form.formId (Pointer.toString control.scope))
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
    fieldGroup (Input.textInput options fieldState [ Attrs.type_ inputType ])
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
                    , Attrs.id (Input.inputElementId fieldState.formId fieldState.pointer)
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
    label [ Attrs.for (Input.inputElementId fieldState.formId fieldState.pointer) ]
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
    label [ Attrs.for (Input.inputElementId fieldState.formId fieldState.pointer) ]
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
                    ]
                    [ text (func f.pointer err) ]
            )
