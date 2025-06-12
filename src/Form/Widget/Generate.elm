module Form.Widget.Generate exposing (widget)

import Dict
import Form.FieldValue as FieldValue exposing (FieldValue, fromFloatInput, fromIntInput, fromStringInput)
import Form.State as F exposing (Form, FormState, Msg(..), validateWidget)
import Form.Widget exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions as Schema exposing (Schema, SingleType(..), SubSchema, Type(..))
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


widget : Form -> Widget
widget form =
    Maybe.withDefault
        (WVerticalLayout [])
        (goWidget form { uiPath = [], disabled = False, uiSchema = form.uiSchema })


goWidget : Form -> UiState -> Maybe Widget
goWidget form uiState =
    let
        ruleEffect : Maybe Rule.AppliedEffect
        ruleEffect =
            Rule.computeRule form.state.value (UI.getRule uiState.uiSchema)

        newUiState =
            { uiState | disabled = ruleEffect == Just Rule.Disabled }
    in
    Maybe.andThen (maybeHide ruleEffect) <|
        case uiState.uiSchema of
            UI.UiHorizontalLayout hl ->
                Just <| horizontalLayoutWidget form newUiState hl

            UI.UiVerticalLayout vl ->
                Just <| verticalLayoutWidget form newUiState vl

            UI.UiGroup g ->
                Just <| groupWidget form newUiState g

            UI.UiControl c ->
                Maybe.andThen
                    (\subSchema ->
                        controlWidget form.defaultOptions newUiState form.schema subSchema c form.state
                    )
                <|
                    UI.pointToSubSchema form.schema c.scope

            UI.UiCategorization c ->
                Just <| categorizationWidget form newUiState c

            UI.UiLabel l ->
                Just <| labelWidget l


maybeHide : Maybe Rule.AppliedEffect -> Widget -> Maybe Widget
maybeHide effect x =
    case effect of
        Just Rule.Hidden ->
            Nothing

        Just Rule.Disabled ->
            Just x

        Nothing ->
            Just x


horizontalLayoutWidget : Form -> UiState -> UI.HorizontalLayout -> Widget
horizontalLayoutWidget form uiState hl =
    WHorizontalLayout <| widgetList form uiState hl.elements


verticalLayoutWidget : Form -> UiState -> UI.VerticalLayout -> Widget
verticalLayoutWidget form uiState hl =
    WVerticalLayout <| widgetList form uiState hl.elements


groupWidget : Form -> UiState -> UI.Group -> Widget
groupWidget form uiState group =
    WGroup
        { label = group.label
        , elements = widgetList form uiState group.elements
        }


widgetList : Form -> UiState -> List UI.UiSchema -> List Widget
widgetList form uiState =
    List.filterMap identity
        << List.indexedMap
            (\ix us ->
                goWidget form (walkState ix us uiState)
            )


categorizationWidget : Form -> UiState -> UI.Categorization -> Widget
categorizationWidget form uiState categorization =
    let
        focusedCategoryIx =
            Maybe.withDefault 0 <| Dict.get uiState.uiPath form.state.categoryFocus

        categoryButton ix cat =
            if Rule.computeRule form.state.value cat.rule == Just Rule.Hidden then
                Nothing

            else
                Just
                    { label = cat.label
                    , focus = focusedCategoryIx == ix
                    , onClick = FocusCategory uiState.uiPath ix
                    }

        categoryUiState cat =
            walkState focusedCategoryIx (UI.UiVerticalLayout { elements = cat.elements, rule = cat.rule }) uiState
    in
    WCategorization
        { buttons = Maybe.values <| List.indexedMap categoryButton categorization.elements
        , elements =
            Maybe.unwrap
                []
                (\cat -> widgetList form (categoryUiState cat) cat.elements)
                (List.getAt focusedCategoryIx categorization.elements)
        }


labelWidget : UI.Label -> Widget
labelWidget l =
    WLabel l.text


controlWidget : UI.DefOptions -> UiState -> Schema -> SubSchema -> UI.Control -> FormState -> Maybe Widget
controlWidget defaultOptions uiState wholeSchema subSchema control form =
    let
        defOptions =
            UI.applyDefaults defaultOptions control.options

        elementId =
            inputElementId form.formId control.scope

        controlOptions =
            let
                disabled =
                    defOptions.readonly || uiState.disabled

                dispRequired =
                    isRequired wholeSchema control.scope && not defOptions.hideRequiredAsterisk

                validation =
                    if validateWidget control.scope form.validateWidgets then
                        case F.getErrorAt control.scope form.errors of
                            Just e ->
                                Invalid e

                            Nothing ->
                                Valid

                    else
                        NotValidated

                label =
                    fieldLabel control.label subSchema control.scope

                isCheckbox =
                    case controlBody of
                        Just (CCheckbox _) ->
                            True

                        _ ->
                            False

                showDescription =
                    if isCheckbox || defOptions.showUnfocusedDescription || form.focus == Just control.scope then
                        subSchema.description

                    else
                        Nothing
            in
            Maybe.map
                (\lbl ->
                    { id = elementId
                    , label = lbl.label
                    , hideLabel = lbl.hideLabel
                    , disabled = disabled
                    , validation = validation
                    , required = dispRequired
                    , description = showDescription
                    , onFocus = Focus control.scope
                    , onBlur = Blur control.scope
                    , trim = defOptions.trim
                    }
                )
                label

        pointedValue =
            Maybe.withDefault (FieldValue.String "") <|
                FieldValue.pointedFieldValue control.scope form.value

        controlBody : Maybe Control
        controlBody =
            case subSchema.type_ of
                SingleType IntegerType ->
                    Just <| textLikeControl IntField pointedValue control.scope elementId defOptions subSchema

                SingleType NumberType ->
                    Just <| textLikeControl NumberField pointedValue control.scope elementId defOptions subSchema

                SingleType StringType ->
                    Just <| textLikeControl (StringField <| formatFromSchema subSchema.format) pointedValue control.scope elementId defOptions subSchema

                SingleType BooleanType ->
                    Just <|
                        CCheckbox
                            { value = FieldValue.asBool pointedValue
                            , onCheck = Input control.scope << FieldValue.Bool
                            , toggle = defOptions.toggle
                            }

                _ ->
                    Nothing
    in
    Maybe.map2 WControl controlOptions controlBody


textLikeControl : FieldType -> FieldValue -> Pointer -> String -> UI.DefOptions -> SubSchema -> Control
textLikeControl fieldType fieldValue pointer elementId defOptions subSchema =
    if subSchema.enum /= Nothing then
        if defOptions.format == Just UI.Radio then
            CRadioGroup
                { valueList =
                    Maybe.toList subSchema.enum
                        |> List.concat
                        |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "")
                        |> List.map
                            (\label ->
                                { id = elementId ++ "-" ++ label
                                , label = label
                                , checked = FieldValue.asString fieldValue == label
                                , onClick = Input pointer <| fromFieldInput fieldType label
                                }
                            )
                , vertical = defOptions.orientation == UI.Vertical
                }

        else
            CSelect
                { valueList =
                    Maybe.toList subSchema.enum
                        |> List.concat
                        |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "")
                        |> List.append [ "" ]
                        |> List.map
                            (\label ->
                                { label = label
                                , selected = FieldValue.asString fieldValue == label
                                }
                            )
                , onChange = Input pointer << fromFieldInput fieldType
                }

    else if defOptions.slider then
        let
            step =
                Maybe.withDefault 1.0 subSchema.multipleOf

            minimum =
                Maybe.withDefault 1.0 subSchema.minimum

            maximum =
                Maybe.withDefault 10.0 subSchema.maximum

            minLimit =
                case subSchema.exclusiveMinimum of
                    Just (Schema.BoolBoundary False) ->
                        minimum

                    Just (Schema.BoolBoundary True) ->
                        minimum + step

                    Just (Schema.NumberBoundary x) ->
                        x + step

                    _ ->
                        minimum

            maxLimit =
                case subSchema.exclusiveMaximum of
                    Just (Schema.BoolBoundary True) ->
                        maximum - step

                    Just (Schema.NumberBoundary x) ->
                        x - step

                    _ ->
                        maximum
        in
        CSlider
            { value = FieldValue.asString fieldValue
            , onInput = Input pointer << fromFieldInput fieldType
            , min = String.fromFloat minLimit
            , max = String.fromFloat maxLimit
            , step = String.fromFloat step
            }

    else if defOptions.multi && isStringField fieldType then
        CTextArea
            { value = FieldValue.asString fieldValue
            , maxLength =
                if defOptions.restrict then
                    subSchema.maxLength

                else
                    Nothing
            , onInput = Input pointer << fromFieldInput fieldType
            }

    else
        CTextInput
            { value = FieldValue.asString fieldValue
            , onInput = Input pointer << fromFieldInput fieldType
            , fieldType = fieldType
            , maxLength =
                if defOptions.restrict then
                    subSchema.maxLength

                else
                    Nothing
            }


{-| Approximate whether a control is required to display asterix in the label
-}
isRequired : Schema -> Pointer -> Bool
isRequired wholeSchema pointer =
    let
        elementSchema =
            UI.pointToSubSchema wholeSchema pointer

        isCheckboxRequired =
            case elementSchema of
                Just schema ->
                    schema.type_ == SingleType BooleanType && schema.const == Just (Encode.bool True)

                Nothing ->
                    False

        parentSchema =
            UI.pointToSubSchema wholeSchema (List.take (List.length pointer - 2) pointer)

        isPropertyRequired =
            case ( parentSchema, List.last pointer ) of
                ( Just schema, Just prop ) ->
                    List.member prop (Maybe.withDefault [] schema.required)

                _ ->
                    False
    in
    isCheckboxRequired || isPropertyRequired


fieldLabel : Maybe UI.ControlLabel -> SubSchema -> Pointer -> Maybe { label : String, hideLabel : Bool }
fieldLabel label schema scope =
    let
        fallbackLabel =
            schema.title
                |> Maybe.orElse (List.last scope |> Maybe.map UI.fieldNameToTitle)
    in
    Maybe.map
        (\fallback ->
            case label of
                Just (UI.StringLabel s) ->
                    { label = s, hideLabel = False }

                Just (UI.BoolLabel False) ->
                    { label = fallback, hideLabel = True }

                Just (UI.BoolLabel True) ->
                    { label = fallback, hideLabel = False }

                Nothing ->
                    { label = fallback, hideLabel = False }
        )
        fallbackLabel


inputElementId : String -> Pointer -> String
inputElementId formId pointer =
    formId ++ "-" ++ Pointer.toString pointer ++ "-input"


fromFieldInput : FieldType -> String -> FieldValue
fromFieldInput fieldType =
    case fieldType of
        StringField _ ->
            fromStringInput

        IntField ->
            fromIntInput

        NumberField ->
            fromFloatInput


formatFromSchema : Maybe String -> FieldFormat
formatFromSchema =
    Maybe.withDefault Text
        << Maybe.map
            (\f ->
                case f of
                    "email" ->
                        Email

                    "date" ->
                        Date

                    "time" ->
                        Time

                    "date-time" ->
                        DateTime

                    "phone" ->
                        Phone

                    _ ->
                        Text
            )


isStringField : FieldType -> Bool
isStringField fieldType =
    case fieldType of
        StringField _ ->
            True

        _ ->
            False
