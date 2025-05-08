module UiSchema.Internal exposing
    ( Categorization
    , Category
    , Condition
    , Control
    , ControlLabel(..)
    , DefOptions
    , Detail
    , Effect(..)
    , ElementLabelProp
    , Format(..)
    , Group
    , HorizontalLayout
    , Label
    , Options
    , Orientation(..)
    , Rule
    , UiSchema(..)
    , VerticalLayout
    , applyDefaults
    , decodeStringLike
    , decodeUiSchema
    , defaultOptions
    , defaultValue
    , fieldNameToTitle
    , generateUiSchema
    , getRule
    , pointToSchema
    , pointToSubSchema
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Json.Pointer as Pointer exposing (Pointer)
import Json.Schema.Definitions as Schema exposing (Schema, SubSchema)
import Json.Util as Util
import Maybe.Extra as Maybe
import String.Case


type UiSchema
    = UiControl Control
    | UiHorizontalLayout HorizontalLayout
    | UiVerticalLayout VerticalLayout
    | UiGroup Group
    | UiCategorization Categorization
    | UiLabel Label


type alias Control =
    { scope : Pointer
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
    { label : Maybe String
    , elements : List UiSchema
    , rule : Maybe Rule
    }


type alias Label =
    -- TODO: determine remaining props here
    { text : String
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
    { scope : Pointer
    , schema : Schema
    }


type alias Options =
    { format : Maybe Format
    , orientation : Maybe Orientation
    , showSortButtons : Maybe Bool
    , detail : Maybe Detail
    , elementLabelProp : Maybe ElementLabelProp
    , readonly : Maybe Bool
    , multi : Maybe Bool
    , slider : Maybe Bool
    , trim : Maybe Bool
    , restrict : Maybe Bool
    , showUnfocusedDescription : Maybe Bool
    , hideRequiredAsterisk : Maybe Bool
    , toggle : Maybe Bool
    , variant : Maybe CategorizationVariant -- TODO: implement
    , showNavButtons : Maybe Bool -- TODO: implement
    }


type alias DefOptions =
    { format : Maybe Format
    , orientation : Orientation
    , showSortButtons : Bool
    , detail : Detail
    , elementLabelProp : Maybe ElementLabelProp
    , readonly : Bool
    , multi : Bool
    , slider : Bool
    , trim : Bool
    , restrict : Bool
    , showUnfocusedDescription : Bool
    , hideRequiredAsterisk : Bool
    , toggle : Bool
    , variant : Maybe CategorizationVariant
    , showNavButtons : Bool
    }


defaultOptions : DefOptions
defaultOptions =
    { format = Nothing
    , orientation = Horizontal
    , showSortButtons = False
    , detail = DetailDefault
    , elementLabelProp = Nothing
    , readonly = False
    , multi = False
    , slider = False
    , trim = False
    , restrict = False
    , showUnfocusedDescription = False
    , hideRequiredAsterisk = False
    , toggle = False
    , variant = Nothing
    , showNavButtons = False
    }


applyDefaults : DefOptions -> Maybe Options -> DefOptions
applyDefaults d mo =
    Maybe.unwrap d
        (\o ->
            { format = Maybe.or o.format d.format
            , orientation = Maybe.withDefault d.orientation o.orientation
            , showSortButtons = Maybe.withDefault d.showSortButtons o.showSortButtons
            , detail = Maybe.withDefault d.detail o.detail
            , elementLabelProp = Maybe.or o.elementLabelProp d.elementLabelProp
            , readonly = Maybe.withDefault d.readonly o.readonly
            , multi = Maybe.withDefault d.multi o.multi
            , slider = Maybe.withDefault d.slider o.slider
            , trim = Maybe.withDefault d.trim o.trim
            , restrict = Maybe.withDefault d.restrict o.restrict
            , showUnfocusedDescription = Maybe.withDefault d.showUnfocusedDescription o.showUnfocusedDescription
            , hideRequiredAsterisk = Maybe.withDefault d.hideRequiredAsterisk o.hideRequiredAsterisk
            , toggle = Maybe.withDefault d.toggle o.toggle
            , variant = Maybe.or o.variant d.variant
            , showNavButtons = Maybe.withDefault d.showNavButtons o.showNavButtons
            }
        )
        mo


type Format
    = Radio


type Orientation
    = Horizontal
    | Vertical


type ElementLabelProp
    = LabelPropString String
    | LabelPropList (List String)


type Detail
    = DetailDefault
    | DetailGenerated
    | DetailRegistered
    | DetailInlined UiSchema


type CategorizationVariant
    = Stepper


decodeUiSchema : Decoder UiSchema
decodeUiSchema =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\ty ->
                case ty of
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

                    "Label" ->
                        Decode.map UiLabel decodeLabel

                    _ ->
                        Decode.fail "Unable to decode UiSchema"
            )


decodeControl : Decoder Control
decodeControl =
    Decode.map4 Control
        (Decode.field "scope" Pointer.decode)
        (Decode.maybe <| Decode.field "label" decodeControlLabel)
        (Decode.maybe <| Decode.field "options" decodeOptions)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeHorizontalLayout : Decoder HorizontalLayout
decodeHorizontalLayout =
    Decode.map2 HorizontalLayout
        (Decode.field "elements" <| Decode.list decodeUiSchema)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeVerticalLayout : Decoder VerticalLayout
decodeVerticalLayout =
    Decode.map2 VerticalLayout
        (Decode.field "elements" <| Decode.list decodeUiSchema)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeGroup : Decoder Group
decodeGroup =
    Decode.map3 Group
        (Decode.maybe <| Decode.field "label" Decode.string)
        (Decode.field "elements" <| Decode.list decodeUiSchema)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeLabel : Decoder Label
decodeLabel =
    Decode.map Label
        (Decode.field "text" Decode.string)


decodeCategorization : Decoder Categorization
decodeCategorization =
    Decode.map2 Categorization
        (Decode.field "elements" <| Decode.list decodeCategory)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeCategory : Decoder Category
decodeCategory =
    Decode.map3 Category
        (Decode.field "label" Decode.string)
        (Decode.field "elements" <| Decode.list decodeUiSchema)
        (Decode.maybe <| Decode.field "rule" decodeRule)


decodeControlLabel : Decoder ControlLabel
decodeControlLabel =
    Decode.oneOf
        [ Decode.map StringLabel Decode.string
        , Decode.map BoolLabel Decode.bool
        ]


decodeRule : Decoder Rule
decodeRule =
    Decode.map2 Rule
        (Decode.field "effect" decodeEffect)
        (Decode.field "condition" decodeCondition)


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
        (Decode.field "scope" Pointer.decode)
        (Decode.field "schema" Schema.decoder)


decodeOptions : Decoder Options
decodeOptions =
    Decode.succeed Options
        |> Decode.optional "format" (Decode.nullable decodeFormat) Nothing
        |> Decode.optional "orientation" (Decode.nullable decodeOrientation) Nothing
        |> Decode.optional "showSortButtons" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "detail" (Decode.nullable decodeDetail) Nothing
        |> Decode.optional "elementLabelProp" (Decode.nullable decodeElementLabelProp) Nothing
        |> Decode.optional "readonly" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "multi" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "slider" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "trim" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "restrict" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "showUnfocusedDescription" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "hideRequiredAsterisk" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "toggle" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "variant" (Decode.nullable decodeVariant) Nothing
        |> Decode.optional "showNavButtons" (Decode.nullable Decode.bool) Nothing


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


decodeOrientation : Decoder Orientation
decodeOrientation =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "horizontal" ->
                        Decode.succeed Horizontal

                    "vertical" ->
                        Decode.succeed Vertical

                    _ ->
                        Decode.fail "Unable to decode Orientation"
            )


decodeVariant : Decoder CategorizationVariant
decodeVariant =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "stepper" ->
                        Decode.succeed Stepper

                    _ ->
                        Decode.fail "Unable to decode Variant"
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


fieldNameToTitle : String -> String
fieldNameToTitle =
    String.Case.convertCase " " True True


generateUiSchema : Schema -> UiSchema
generateUiSchema schema =
    let
        go : Maybe String -> Schema -> Pointer -> Maybe UiSchema
        go mName s p =
            Util.withObjectSchema Nothing s <|
                \o ->
                    case o.type_ of
                        Schema.SingleType t ->
                            case t of
                                Schema.ObjectType ->
                                    let
                                        elements =
                                            List.filterMap (\( name, x ) -> go (Just name) x (List.append p [ "properties", name ])) (Util.getProperties o)
                                    in
                                    Just <|
                                        case mName of
                                            Nothing ->
                                                UiVerticalLayout
                                                    { elements = elements
                                                    , rule = Nothing
                                                    }

                                            Just name ->
                                                UiGroup
                                                    { label = Just (fieldNameToTitle name)
                                                    , elements = elements
                                                    , rule = Nothing
                                                    }

                                Schema.NullType ->
                                    Nothing

                                _ ->
                                    Just <|
                                        UiControl
                                            { scope = p
                                            , label = Nothing
                                            , options = Nothing
                                            , rule = Nothing
                                            }

                        _ ->
                            Nothing
    in
    Maybe.withDefault (UiVerticalLayout { elements = [], rule = Nothing }) <| go Nothing schema []


pointToSchema : Schema -> Pointer -> Maybe Schema
pointToSchema schema pointer =
    case pointer of
        [] ->
            Just schema

        "properties" :: x :: xs ->
            Util.withObjectSchema Nothing schema <|
                \os ->
                    case os.properties of
                        Nothing ->
                            Nothing

                        Just (Schema.Schemata props) ->
                            Maybe.andThen (\( _, p ) -> pointToSchema p xs) <| List.head <| List.filter (\( n, _ ) -> n == x) props

        _ ->
            Nothing


pointToSubSchema : Schema -> Pointer -> Maybe SubSchema
pointToSubSchema schema =
    pointToSchema schema
        >> Maybe.andThen
            (\s ->
                case s of
                    Schema.ObjectSchema subSchema ->
                        Just subSchema

                    _ ->
                        Nothing
            )


getRule : UiSchema -> Maybe Rule
getRule uiSchema =
    case uiSchema of
        UiControl x ->
            x.rule

        UiHorizontalLayout x ->
            x.rule

        UiVerticalLayout x ->
            x.rule

        UiGroup x ->
            x.rule

        UiCategorization x ->
            x.rule

        UiLabel _ ->
            Nothing


decodeStringLike : Decode.Decoder String
decodeStringLike =
    Decode.oneOf
        [ Decode.string
        , Decode.int |> Decode.map String.fromInt
        , Decode.float |> Decode.map String.fromFloat
        ]


{-| Properties which are by default not omitted, but created with a default value.
-}
defaultedProps : Schema.SubSchema -> List ( String, Value )
defaultedProps schema =
    Util.getProperties schema
        |> List.filter
            (\( _, v ) ->
                Util.withObjectSchema False v <|
                    \o_ ->
                        List.member o_.type_ [ Schema.SingleType Schema.BooleanType, Schema.SingleType Schema.ObjectType ]
            )
        |> List.map (\( k, v ) -> ( k, defaultValue v ))
        |> List.sortBy (\( k, _ ) -> k)


{-| Produce a default value for the given form schema.
-}
defaultValue : Schema -> Value
defaultValue schema =
    case defaultObjectSchemaValue schema of
        Nothing ->
            Util.withObjectSchema Encode.null schema <|
                \o ->
                    case o.type_ of
                        Schema.SingleType Schema.ObjectType ->
                            Encode.object (defaultedProps o)

                        Schema.SingleType Schema.ArrayType ->
                            Encode.list Encode.int []

                        Schema.SingleType Schema.IntegerType ->
                            Encode.int 0

                        Schema.SingleType Schema.NumberType ->
                            Encode.float 0

                        Schema.SingleType Schema.StringType ->
                            Encode.string ""

                        Schema.SingleType Schema.BooleanType ->
                            Encode.bool False

                        _ ->
                            Encode.null

        Just v ->
            v


defaultObjectSchemaValue : Schema -> Maybe Value
defaultObjectSchemaValue schema =
    Util.withObjectSchema Nothing schema <|
        \o ->
            case o.default of
                Just d ->
                    Just d

                Nothing ->
                    case o.type_ of
                        Schema.SingleType t ->
                            case t of
                                Schema.ObjectType ->
                                    let
                                        schemata =
                                            Util.getProperties o

                                        propDefault ( name, sch ) =
                                            Maybe.map (\s -> ( name, s )) <| defaultObjectSchemaValue sch

                                        propDefaults =
                                            List.filterMap propDefault schemata
                                    in
                                    if propDefaults == [] then
                                        Nothing

                                    else
                                        Just <| Encode.object propDefaults

                                Schema.ArrayType ->
                                    Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
