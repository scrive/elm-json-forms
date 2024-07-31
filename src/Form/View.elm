module Form.View exposing (view)

import Dict
import Form.Error exposing (ErrorValue)
import Form.FieldValue as FieldValue exposing (FieldType(..))
import Form.Settings exposing (Settings)
import Form.State as F exposing (Form, FormState, Msg(..))
import Form.Theme exposing (Theme)
import Form.View.Input as Input exposing (Input)
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes as Attrs
import Html.Attributes.Extra as Attrs
import Html.Events exposing (..)
import Html.Extra as Html
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions exposing (Schema(..), SingleType(..), SubSchema, Type(..))
import Json.Util as Util
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
    maybeHide ruleEffect <|
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


maybeHide : Maybe Rule.AppliedEffect -> List (Html F.Msg) -> List (Html F.Msg)
maybeHide effect x =
    case effect of
        Just Rule.Hidden ->
            []

        Just Rule.Disabled ->
            x

        Nothing ->
            x


horizontalLayoutView : Form -> UiState -> UI.HorizontalLayout -> List (Html F.Msg)
horizontalLayoutView form uiState hl =
    [ div [ Attrs.map never <| form.settings.theme.horizontalLayout { cols = List.length hl.elements } ] <|
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
        (Decode.succeed ( msg, True ))


categorizationView : Form -> UiState -> UI.Categorization -> List (Html F.Msg)
categorizationView form uiState categorization =
    let
        focusedCategoryIx =
            Maybe.withDefault 0 <| Dict.get uiState.uiPath form.state.categoryFocus

        categoryButton ix cat =
            if Rule.computeRule form.state.value cat.rule == Just Rule.Hidden then
                Nothing

            else
                Just <|
                    button
                        [ Attrs.map never <| form.settings.theme.categorizationMenuItem { focus = focusedCategoryIx == ix }
                        , onClickPreventDefault <| F.FocusCategory uiState.uiPath ix
                        ]
                        [ text cat.label ]

        categoryUiState cat =
            walkState focusedCategoryIx (UI.UiVerticalLayout { elements = cat.elements, rule = cat.rule }) uiState
    in
    div
        [ Attrs.map never form.settings.theme.categorizationMenu
        ]
        (Maybe.values <| List.indexedMap categoryButton categorization.elements)
        :: Maybe.unwrap [] (\cat -> view form (categoryUiState cat)) (List.getAt focusedCategoryIx categorization.elements)


{-| Approximate whether a control is required to display asterix in the label
-}
isRequired : Schema -> Pointer -> Bool
isRequired wholeSchema pointer =
    let
        parentSchema =
            UI.pointToSchema wholeSchema (List.take (List.length pointer - 2) pointer)
    in
    case ( parentSchema, List.last pointer ) of
        ( Just (ObjectSchema schema), Just prop ) ->
            case schema.type_ of
                SingleType ObjectType ->
                    List.any ((==) prop) (Maybe.withDefault [] schema.required)

                _ ->
                    False

        _ ->
            False


controlView : Settings -> UiState -> Schema -> UI.Control -> FormState -> Html F.Msg
controlView settings uiState wholeSchema control form =
    let
        controlSchema =
            UI.pointToSchema wholeSchema control.scope

        defOptions =
            UI.applyDefaults control.options

        disabled =
            defOptions.readonly == True || uiState.disabled

        required =
            isRequired wholeSchema control.scope

        fieldState =
            F.fieldState disabled required control.scope form

        controlBody schema_ =
            Util.withObjectSchema Html.nothing schema_ <|
                \schema ->
                    case schema.type_ of
                        SingleType IntegerType ->
                            textLikeInput settings control defOptions schema IntField fieldState

                        SingleType NumberType ->
                            textLikeInput settings control defOptions schema NumberField fieldState

                        SingleType StringType ->
                            textLikeInput settings control defOptions schema StringField fieldState

                        SingleType BooleanType ->
                            checkbox settings control defOptions schema fieldState

                        _ ->
                            Html.nothing
    in
    Html.viewMaybe
        (\cs ->
            div
                [ Attrs.id (Input.inputElementGroupId form.formId (Pointer.toString control.scope))
                , Attrs.map never settings.theme.fieldGroup
                , if fieldState.disabled then
                    Attrs.map never settings.theme.disabledElems

                  else
                    Attrs.empty
                ]
                [ controlBody cs ]
        )
        controlSchema


textLikeInput : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> FieldType -> Input
textLikeInput settings control defOptions schema fieldType state =
    if schema.enum /= Nothing then
        if defOptions.format == Just UI.Radio then
            radioGroup settings control defOptions schema fieldType state

        else
            select settings control defOptions schema fieldType state

    else if defOptions.slider == True then
        slider settings control defOptions schema fieldType state

    else if defOptions.multi && fieldType == StringField then
        textarea settings control defOptions schema state

    else
        textInput settings control defOptions schema fieldType state


textInput : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> FieldType -> Input
textInput settings control defOptions schema fieldType fieldState =
    let
        inputType : String
        inputType =
            case fieldType of
                StringField ->
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

                NumberField ->
                    "number"

                IntField ->
                    "number"
    in
    fieldGroup (Input.baseTextInput settings defOptions fieldType inputType schema.maxLength fieldState)
        settings
        { showLabel = True }
        control
        defOptions
        schema
        fieldState


slider : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> FieldType -> Input
slider settings control defOptions schema fieldType fieldState =
    fieldGroup (Input.slider settings defOptions schema fieldType fieldState)
        settings
        { showLabel = True }
        control
        defOptions
        schema
        fieldState


textarea : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> Input
textarea settings control defOptions schema state =
    fieldGroup
        (Input.textArea settings defOptions schema.maxLength state)
        settings
        { showLabel = True }
        control
        defOptions
        schema
        state


select : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> FieldType -> F.FieldState -> Html F.Msg
select settings control defOptions schema fieldType fieldState =
    let
        values : List String
        values =
            Maybe.toList schema.enum |> List.concat |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "") |> List.append [ "" ]

        items : List ( String, String )
        items =
            List.map (\v -> ( v, v )) values
    in
    fieldGroup
        (Input.baseSelectInput fieldType settings items fieldState)
        settings
        { showLabel = True }
        control
        defOptions
        schema
        fieldState


radioGroup : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> FieldType -> Input
radioGroup settings control defOptions schema fieldType fieldState =
    let
        values : List String
        values =
            Maybe.toList schema.enum |> List.concat |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "")

        elementId =
            Input.inputElementId fieldState.formId fieldState.pointer

        radio value =
            let
                optionElementId =
                    elementId ++ "-" ++ value
            in
            label [ Attrs.for optionElementId, Attrs.map never <| settings.theme.radioEntry { vertical = defOptions.orientation == UI.Vertical } ]
                [ Html.input
                    [ Attrs.type_ "radio"
                    , Attrs.id optionElementId
                    , Attrs.name elementId
                    , Attrs.checked <| value == FieldValue.asString fieldState.value
                    , Attrs.map never settings.theme.radioInput
                    , onClick (Input fieldState.pointer (FieldValue.fromFieldInput fieldType value))
                    , onFocus (Focus fieldState.pointer)
                    , onBlur Blur
                    , Attrs.disabled fieldState.disabled
                    ]
                    []
                , span [ Attrs.map never settings.theme.fieldLabel ] [ text value ]
                ]
    in
    fieldGroup
        (div [] (List.map radio values))
        settings
        { showLabel = True }
        control
        defOptions
        schema
        fieldState


checkbox : Settings -> UI.Control -> UI.DefOptions -> SubSchema -> Input
checkbox settings control defOptions schema fieldState =
    let
        inputField : Html F.Msg
        inputField =
            div [ Attrs.map never settings.theme.checkboxRow ]
                [ if (control.options |> Maybe.andThen .toggle) == Just True then
                    Input.toggleInput settings fieldState

                  else
                    Input.checkboxInput settings fieldState
                , Html.viewMaybe identity <| fieldLabel settings.theme control.label defOptions schema control.scope (fieldState.required && schema.const == Just (Encode.bool True))
                ]
    in
    fieldGroup
        inputField
        settings
        { showLabel = False }
        control
        defOptions
        schema
        fieldState


fieldGroup : Html F.Msg -> Settings -> { showLabel : Bool } -> UI.Control -> UI.DefOptions -> SubSchema -> Input
fieldGroup inputField settings { showLabel } control defOptions schema fieldState =
    let
        label_ : Maybe (Html F.Msg)
        label_ =
            if showLabel then
                fieldLabel settings.theme control.label defOptions schema control.scope fieldState.required

            else
                Nothing

        showDescription =
            defOptions.showUnfocusedDescription == True || fieldState.hasFocus

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
        [ Html.viewMaybe identity label_
        , inputField
        , description
        , errorMessage
        ]


fieldLabel : Theme -> Maybe UI.ControlLabel -> UI.DefOptions -> SubSchema -> Pointer -> Bool -> Maybe (Html F.Msg)
fieldLabel theme label options schema scope required =
    let
        fallback =
            schema.title
                |> Maybe.orElse (List.last scope |> Maybe.map UI.fieldNameToTitle)
                |> Maybe.withDefault ""

        render str =
            span [ Attrs.map never theme.fieldLabel ]
                [ text
                    (if not options.hideRequiredAsterisk && required then
                        str ++ " *"

                     else
                        str
                    )
                ]
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
