module Json.Schema.Form.UiSchema exposing
    ( Categorization
    , Category
    , Control
    , Group
    , HorizontalLayout
    , UiSchema(..)
    , Rule
    , Effect (..)
    , Condition
    , VerticalLayout
    , decodeStringLike
    , defaultValue
    , fromString
    , generateUiSchema
    , pointToSchema
    , unSchemata
    , fieldNameToTitle
    , getRule
    )

import Dict exposing (Dict)
import Form.Field exposing (FieldValue(..))
import Form.Pointer as Pointer exposing (Pointer)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import String.Case
import Json.Encode as Encode
import Json.Schema.Definitions as Schema exposing (Schema(..))


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
    { detail : Maybe Detail
    , showSortButtons : Maybe Bool
    , elementLabelProp : Maybe ElementLabelProp
    , format : Maybe Format
    , readonly : Maybe Bool
    , multi : Maybe Bool
    , slider : Maybe Bool
    , trim : Maybe Bool -- TODO: implement
    , restrict : Maybe Bool -- TODO: implement
    , showUnfocusedDescription : Maybe Bool
    , hideRequiredAsterisk : Maybe Bool -- TODO: implement
    , toggle : Maybe Bool -- TODO: implement
    , variant : Maybe String -- TODO: implement
    , showNavButtons : Maybe String -- TODO: implement
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
        |> Decode.optional "detail" (Decode.nullable decodeDetail) Nothing
        |> Decode.optional "showSortButtons" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "elementLabelProp" (Decode.nullable decodeElementLabelProp) Nothing
        |> Decode.optional "format" (Decode.nullable decodeFormat) Nothing
        |> Decode.optional "readonly" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "multi" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "slider" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "trim" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "restrict" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "showUnfocusedDescription" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "hideRequiredAsterisk" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "toggle" (Decode.nullable Decode.bool) Nothing
        |> Decode.optional "variant" (Decode.nullable Decode.string) Nothing
        |> Decode.optional "showNavButtons" (Decode.nullable Decode.string) Nothing


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


fieldNameToTitle : String -> String
fieldNameToTitle = String.Case.convertCase " " True True

generateUiSchema : Schema -> UiSchema
generateUiSchema schema =
    let
        go : Maybe String -> Schema -> Pointer -> Maybe UiSchema
        go mName s p =

                case s of
                    Schema.BooleanSchema _ ->
                        Nothing

                    Schema.ObjectSchema o ->
                        case o.type_ of
                            Schema.SingleType t ->
                                case t of
                                    Schema.ObjectType ->
                                        let
                                            elements = List.filterMap (\(name, x) -> go (Just name) x (List.append p [ "properties", name ])) (Maybe.withDefault [] <| Maybe.map unSchemata o.properties)
                                        in Just <| case mName of
                                            Nothing -> UiVerticalLayout
                                                { elements = elements
                                                , rule = Nothing
                                                }
                                            Just name -> UiGroup
                                                { label = Just (fieldNameToTitle name)
                                                , elements = elements
                                                , rule = Nothing
                                                }

                                    Schema.NullType ->
                                        Nothing

                                    _ ->
                                        Just <| UiControl
                                        { scope = p
                                        , label = Nothing
                                        , options = Nothing
                                        , rule = Nothing
                                        }

                            _ ->
                                Nothing

    in
        Maybe.withDefault (UiVerticalLayout { elements = [], rule = Nothing }) <| go Nothing schema []


unSchemata : Schema.Schemata -> List ( String, Schema )
unSchemata (Schema.Schemata l) =
    l


pointToSchema : Schema -> Pointer -> Maybe Schema
pointToSchema schema pointer =
    case pointer of
        [] ->
            Just schema

        "properties" :: x :: xs ->
            case schema of
                Schema.BooleanSchema _ ->
                    Nothing

                Schema.ObjectSchema os ->
                    case os.properties of
                        Nothing ->
                            Nothing

                        Just (Schema.Schemata props) ->
                            Maybe.andThen (\( n, p ) -> pointToSchema p xs) <| List.head <| List.filter (\( n, p ) -> n == x) props

        _ ->
            Nothing


allPointers : UiSchema -> List Pointer
allPointers uiSchema =
    case uiSchema of
        UiControl x ->
            [ x.scope ]

        UiHorizontalLayout x ->
            List.concatMap allPointers x.elements

        UiVerticalLayout x ->
            List.concatMap allPointers x.elements

        UiGroup x ->
            List.concatMap allPointers x.elements

        UiCategorization x ->
            List.concatMap allPointers <|
                List.concatMap .elements x.elements

        UiLabel x ->
            []


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

        UiLabel x ->
            Nothing


decodeStringLike : Decode.Decoder String
decodeStringLike =
    Decode.oneOf
        [ Decode.string
        , Decode.int |> Decode.map String.fromInt
        , Decode.float |> Decode.map String.fromFloat
        ]


{-| Properties which are, by default, not omitted but with a default value.
-}
defaultedProps : Schema.SubSchema -> List (String, Value)
defaultedProps schema =
    Maybe.map unSchemata schema.properties
    |> Maybe.withDefault []
    |> List.filter (\(k, v) ->
        case v of
            ObjectSchema o_ -> List.member o_.type_ [Schema.SingleType Schema.BooleanType, Schema.SingleType Schema.ObjectType]
            BooleanSchema _ -> False
        )
    |> List.map (\(k, v) -> (k, defaultValue v))
    |> List.sortBy (\(k, _) -> k)

defaultValue : Schema -> Value
defaultValue schema =
    case defaultValue_ schema of
        Nothing ->
            case schema of
                BooleanSchema _ ->
                    Encode.null

                ObjectSchema o ->
                    case o.type_ of
                        Schema.SingleType Schema.ObjectType -> Encode.object (defaultedProps o)

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


defaultValue_ : Schema -> Maybe Value
defaultValue_ schema =
    case schema of
        BooleanSchema _ ->
            Nothing

        ObjectSchema o ->
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
                                            Maybe.withDefault [] <|
                                                Maybe.map unSchemata o.properties

                                        propDefault ( name, sch ) =
                                            Maybe.map (\s -> ( name, s )) <| defaultValue_ sch

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
