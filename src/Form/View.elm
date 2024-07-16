module Form.View exposing (view)

import Dict
import Form.Error exposing (ErrorValue)
import Form.Settings exposing (Settings)
import Form.State as F exposing (Form, FormState)
import Form.Theme exposing (Theme)
import Form.View.Input as Input
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, preventDefaultOn)
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
    applyEffect form.settings.theme ruleEffect <|
        case uiState.uiSchema of
            UI.UiControl c ->
                [ controlView form.settings newUiState form.schema c form.state ]

            UI.UiHorizontalLayout hl ->
                horizontalLayoutView form newUiState hl

            UI.UiVerticalLayout vl ->
                verticalLayoutView form newUiState vl

            UI.UiGroup g ->
                groupView form newUiState g

            UI.UiCategorization c ->
                categorizationView form newUiState c

            UI.UiLabel l ->
                [ Html.div [ Attrs.map never form.settings.theme.label ] [ text l.text ] ]


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
    [ div [ Attrs.map never form.settings.theme.horizontalLayout ] <|
        List.indexedMap
            (\ix us ->
                div
                    [ Attrs.map never form.settings.theme.horizontalLayoutItem ]
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
            Maybe.unwrap [] (\l -> [ Html.div [ Attrs.map never form.settings.theme.groupLabel ] [ text l ] ]) group.label

        contents =
            verticalLayoutView form uiState { elements = group.elements, rule = group.rule }
    in
    [ div [ Attrs.map never form.settings.theme.group ] (title ++ contents)
    ]


onClickPreventDefault : msg -> Html.Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click"
        (Decode.succeed <| ( msg, True ))


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
                        [ Attrs.map never <| form.settings.theme.categorizationMenuItem { focus = focusedCategoryIx == ix }
                        , onClickPreventDefault <| F.FocusCategory uiState.uiPath ix
                        ]
                        [ text category.label ]

        categoryUiState cat =
            walkState focusedCategoryIx (UI.UiVerticalLayout { elements = cat.elements, rule = cat.rule }) uiState
    in
    div
        [ Attrs.map never form.settings.theme.categorizationMenu
        ]
        (Maybe.values <| List.indexedMap categoryButton categorization.elements)
        :: Maybe.unwrap [] (\cat -> view form (categoryUiState cat)) (List.getAt focusedCategoryIx categorization.elements)


controlView : Settings -> UiState -> Schema -> UI.Control -> FormState -> Html F.Msg
controlView settings uiState wholeSchema control form =
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
                                select settings control schema fieldState IntField

                            else if (control.options |> Maybe.andThen .slider) == Just True then
                                intSlider settings control schema fieldState

                            else
                                intInput settings control schema fieldState

                        SingleType NumberType ->
                            if schema.enum /= Nothing then
                                select settings control schema fieldState NumberField

                            else if (control.options |> Maybe.andThen .slider) == Just True then
                                numberSlider settings control schema fieldState

                            else
                                numberInput settings control schema fieldState

                        SingleType StringType ->
                            if schema.enum /= Nothing then
                                select settings control schema fieldState StringField

                            else if (control.options |> Maybe.andThen .multi) == Just True then
                                textarea settings control schema fieldState

                            else
                                textInput settings control schema fieldState

                        SingleType BooleanType ->
                            checkbox settings control schema fieldState

                        _ ->
                            Html.nothing
    in
    Html.viewMaybe
        (\cs ->
            div
                [ Attrs.id (Input.inputElementGroupId form.formId (Pointer.toString control.scope))
                , Attrs.map never settings.theme.fieldGroup
                ]
                [ controlBody cs ]
        )
        mControlSchema


type TextFieldType
    = NumberField
    | IntField
    | StringField


textInput : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
textInput settings control schema fieldState =
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

        options = Maybe.withDefault UI.emptyOptions control.options
    in
    fieldGroup (Input.textInput settings options fieldState [ Attrs.type_ inputType ])
        settings
        control
        schema
        fieldState


intInput : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
intInput settings control schema fieldState =
    fieldGroup (Input.intInput settings (Maybe.withDefault UI.emptyOptions control.options) fieldState [])
        settings
        control
        schema
        fieldState


numberInput : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
numberInput settings control schema fieldState =
    fieldGroup (Input.floatInput settings (Maybe.withDefault UI.emptyOptions control.options) fieldState [])
        settings
        control
        schema
        fieldState


intSlider : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
intSlider settings control schema fieldState =
    fieldGroup (Input.intSlider settings (Maybe.withDefault UI.emptyOptions control.options) schema fieldState [])
        settings
        control
        schema
        fieldState


numberSlider : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
numberSlider settings control schema fieldState =
    fieldGroup (Input.numberSlider settings (Maybe.withDefault UI.emptyOptions control.options) schema fieldState [])
        settings
        control
        schema
        fieldState


textarea : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
textarea settings control schema state =
    fieldGroup (Input.textArea settings (Maybe.withDefault UI.emptyOptions control.options) state []) settings control schema state


checkbox : Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
checkbox settings control schema fieldState =
    let
        inputField : Html F.Msg
        inputField =
            div [ Attrs.map never settings.theme.checkboxRow ]
                [ Input.checkboxInput fieldState
                    [ Attrs.map never <| settings.theme.checkboxInput { invalid = fieldState.error /= Nothing }
                    , Attrs.id (Input.inputElementId fieldState.formId fieldState.pointer)
                    ]
                , Html.viewMaybe identity <| fieldLabel settings.theme control.label schema control.scope
                ]

        description : Html F.Msg
        description =
            Html.viewMaybe identity <| fieldDescription settings.theme schema

        errorMessage : Html F.Msg
        errorMessage =
            Html.viewMaybe identity <| error settings.theme settings.errors fieldState
    in
    label [ Attrs.for (Input.inputElementId fieldState.formId fieldState.pointer) ]
        [ inputField
        , description
        , errorMessage
        ]


select : Settings -> UI.Control -> SubSchema -> F.FieldState -> TextFieldType -> Html F.Msg
select settings control schema fieldState fieldType =
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
        (inputType settings items fieldState [])
        settings
        control
        schema
        fieldState


fieldGroup : Html F.Msg -> Settings -> UI.Control -> SubSchema -> F.FieldState -> Html F.Msg
fieldGroup inputField settings control schema fieldState =
    let
        title : Maybe (Html F.Msg)
        title =
            fieldLabel settings.theme control.label schema control.scope

        showDescription =
            Maybe.andThen .showUnfocusedDescription control.options == Just True || fieldState.hasFocus

        description : Html F.Msg
        description =
            Html.viewMaybe identity <|
                if showDescription then
                    fieldDescription settings.theme schema

                else
                    Nothing

        errorMessage : Html F.Msg
        errorMessage =
            Html.viewMaybe identity <| error settings.theme settings.errors fieldState
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
