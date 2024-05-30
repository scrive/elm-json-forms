module Json.Schema.Form.UiSchema exposing
    ( UiSchema
    , fromString
    , generateUiSchema
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Schema.Definitions as Schema
import Json.Schema.Form.Pointer as Pointer


type UiSchema
    = UiControl Control
    | UiHorizontalLayout HorizontalLayout
    | UiVerticalLayout VerticalLayout
    | UiGroup Group
    | UiCategorization Categorization


type alias Control =
    { scope : Pointer.Pointer
    , label : Maybe ControlLabel
    , options : Maybe Options
    , rule : Maybe Rule
    }


type alias HorizontalLayout =
    { elements : List UiSchema
    , rule : Maybe Rule
    }


type alias VerticalLayout =
    { elements : List UiSchema
    , rule : Maybe Rule
    }


type alias Group =
    { label : String
    , elements : List UiSchema
    , rule : Maybe Rule
    }


type alias Categorization =
    { elements : List Category
    , rule : Maybe Rule
    }


type alias Category =
    { label : String
    , elements : List UiSchema
    , rule : Maybe Rule
    }


type ControlLabel
    = StringLabel String
    | BoolLabel Bool


type alias Rule =
    { effect : Effect
    , condition : Condition
    }


type Effect
    = EffectHide
    | EffectShow
    | EffectEnable
    | EffectDisable


type alias Condition =
    { scope : Pointer.Pointer
    , schema : Schema.Schema
    }


type alias Options =
    { detail : Maybe Detail
    , showSortButtons : Maybe Bool
    , elementLabelProp : Maybe ElementLabelProp
    , format : Maybe Format
    , readonly : Maybe Bool
    }


type Format
    = Radio


type ElementLabelProp
    = LabelPropString String
    | LabelPropList (List String)


type Detail
    = DetailDefault
    | DetailGenerated
    | DetailRegistered
    | DetailInlined UiSchema


fromString : String -> Result String UiSchema
fromString =
    Decode.decodeString decodeUiSchema >> Result.mapError Decode.errorToString


decodeUiSchema : Decoder UiSchema
decodeUiSchema =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Control" ->
                        Decode.map UiControl decodeControl

                    "HorizontalLayout" ->
                        Decode.map UiHorizontalLayout decodeHorizontalLayout

                    "VerticalLayout" ->
                        Decode.map UiVerticalLayout decodeVerticalLayout

                    "Group" ->
                        Decode.map UiGroup decodeGroup

                    "Categorization" ->
                        Decode.map UiCategorization decodeCategorization

                    _ ->
                        Decode.fail "Unable to decode UiSchema"
            )


decodeControl : Decoder Control
decodeControl =
    Decode.map4 Control
        Pointer.decode
        (Decode.maybe decodeControlLabel)
        (Decode.maybe decodeOptions)
        (Decode.maybe decodeRule)


decodeHorizontalLayout : Decoder HorizontalLayout
decodeHorizontalLayout =
    Decode.map2 HorizontalLayout
        (Decode.list decodeUiSchema)
        (Decode.maybe decodeRule)


decodeVerticalLayout : Decoder VerticalLayout
decodeVerticalLayout =
    Decode.map2 VerticalLayout
        (Decode.list decodeUiSchema)
        (Decode.maybe decodeRule)


decodeGroup : Decoder Group
decodeGroup =
    Decode.map3 Group
        Decode.string
        (Decode.list decodeUiSchema)
        (Decode.maybe decodeRule)


decodeCategorization : Decoder Categorization
decodeCategorization =
    Decode.map2 Categorization
        (Decode.list decodeCategory)
        (Decode.maybe decodeRule)


decodeCategory : Decoder Category
decodeCategory =
    Decode.map3 Category
        Decode.string
        (Decode.list decodeUiSchema)
        (Decode.maybe decodeRule)


decodeControlLabel : Decoder ControlLabel
decodeControlLabel =
    Decode.oneOf
        [ Decode.map StringLabel Decode.string
        , Decode.map BoolLabel Decode.bool
        ]


decodeRule : Decoder Rule
decodeRule =
    Decode.map2 Rule
        decodeEffect
        decodeCondition


decodeEffect : Decoder Effect
decodeEffect =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "ENABLE" ->
                        Decode.succeed EffectEnable

                    "DISABLE" ->
                        Decode.succeed EffectDisable

                    "SHOW" ->
                        Decode.succeed EffectShow

                    "HIDE" ->
                        Decode.succeed EffectHide

                    _ ->
                        Decode.fail "Unable to decode Effect"
            )


decodeCondition : Decoder Condition
decodeCondition =
    Decode.map2 Condition
        Pointer.decode
        Schema.decoder


decodeOptions : Decoder Options
decodeOptions =
    Decode.map5 Options
        (Decode.maybe decodeDetail)
        (Decode.maybe Decode.bool)
        (Decode.maybe decodeElementLabelProp)
        (Decode.maybe decodeFormat)
        (Decode.maybe Decode.bool)


decodeFormat : Decoder Format
decodeFormat =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "radio" ->
                        Decode.succeed Radio

                    _ ->
                        Decode.fail "Unable to decode Format"
            )


decodeElementLabelProp : Decoder ElementLabelProp
decodeElementLabelProp =
    Decode.oneOf
        [ Decode.map LabelPropString Decode.string
        , Decode.map LabelPropList <| Decode.list Decode.string
        ]


decodeDetail : Decoder Detail
decodeDetail =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "default" ->
                        Decode.succeed DetailDefault

                    "generated" ->
                        Decode.succeed DetailGenerated

                    "registered" ->
                        Decode.succeed DetailRegistered

                    "inlined" ->
                        Decode.map DetailInlined decodeUiSchema

                    _ ->
                        Decode.fail "Unable to decode Detail"
            )


generateUiSchema : Schema.Schema -> UiSchema
generateUiSchema schema =
    UiVerticalLayout
        { elements =
            generateControlPointers schema []
                |> List.map
                    (\p ->
                        UiControl
                            { scope = p
                            , label = Nothing
                            , options = Nothing
                            , rule = Nothing
                            }
                    )
        , rule = Nothing
        }


generateControlPointers : Schema.Schema -> Pointer.Pointer -> List Pointer.Pointer
generateControlPointers s p =
    case s of
        Schema.BooleanSchema _ ->
            []

        Schema.ObjectSchema o ->
            case o.type_ of
                Schema.SingleType t ->
                    case t of
                        Schema.ObjectType ->
                            List.concatMap
                                (\( name, schema ) -> generateControlPointers schema (List.append [ name ] p))
                            <|
                                Maybe.withDefault [] <|
                                    Maybe.map unSchemata o.properties

                        Schema.NullType ->
                            []

                        _ ->
                            [ p ]

                _ ->
                    []


unSchemata : Schema.Schemata -> List ( String, Schema.Schema )
unSchemata (Schema.Schemata l) =
    l
