module Json.Schema.Form.Fields exposing (TextFieldType(..), schemaView, uiSchemaView)

import Dict exposing (Dict)
import Form as F
import Form.Error exposing (ErrorValue)
import Form.Field as Field exposing (FieldValue)
import Form.Input as Input
import Form.Pointer as Pointer exposing (Pointer)
import Form.Validate
import Html exposing (Attribute, Html, button, div, label, legend, li, ol, p, span, text)
import Html.Attributes as Attrs
    exposing
        ( attribute
        , autocomplete
        , class
        , classList
        , for
        , id
        , placeholder
        , rows
        , style
        , tabindex
        , type_
        )
import Html.Attributes.Extra as Attr
import Html.Events exposing (preventDefaultOn)
import Html.Extra as Html
import Html.Keyed
import Json.Decode as Decode exposing (Value)
import Json.Schema.Definitions
    exposing
        ( Items(..)
        , Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        , blankSchema
        )
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Options exposing (Options)
import Json.Schema.Form.Theme exposing (Theme)
import Json.Schema.Form.UiSchema as UI exposing (UiSchema)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Case


type alias Form =
    F.Form


uiSchemaView : Options -> List String -> UiSchema -> Schema -> Form -> Html F.Msg
uiSchemaView options uiPath uiSchema schema form =
    case uiSchema of
        UI.UiControl c ->
            controlView options uiPath schema c form

        UI.UiHorizontalLayout hl ->
            div
                -- TODO: simplify options.theme.group
                [ Attrs.map never <| options.theme.formRow
                ]
            <|
                List.map
                    (\us ->
                        div
                            [ Attrs.map never <| options.theme.formRowItem ]
                            [ uiSchemaView options uiPath us schema form ]
                    )
                    hl.elements

        UI.UiVerticalLayout vl ->
            div
                -- TODO: simplify options.theme.group
                []
            <|
                List.map
                    (\us ->
                        div
                            [ Attrs.map never <| options.theme.formRow ]
                            [ div [ Attrs.map never <| options.theme.formRowItem ] [ uiSchemaView options uiPath us schema form ] ]
                    )
                    vl.elements

        UI.UiGroup g ->
            div [] [ text "unimplemented group" ]

        UI.UiCategorization c ->
            div [] [ text "unimplemented categorization" ]

        UI.UiLabel l ->
            div [] [ text l.text ]


controlView : Options -> List String -> Schema -> UI.Control -> Form -> Html F.Msg
controlView options uiPath wholeSchema control form =
    let
        mControlSchema =
            UI.pointToSchema wholeSchema control.scope

        fieldState =
            F.getField (Pointer.toString control.scope) form

        controlBody schema_ =
            case schema_ of
                BooleanSchema _ ->
                    Html.nothing

                ObjectSchema schema ->
                    case schema.type_ of
                        SingleType IntegerType ->
                            if schema.enum /= Nothing then
                                select options control.scope schema fieldState IntField

                            else
                                txt options control.scope schema fieldState IntField

                        SingleType NumberType ->
                            if schema.enum /= Nothing then
                                select options control.scope schema fieldState NumberField

                            else
                                txt options control.scope schema fieldState NumberField

                        SingleType StringType ->
                            if schema.enum /= Nothing then
                                select options control.scope schema fieldState StringField

                            else
                                txt options control.scope schema fieldState StringField

                        SingleType BooleanType ->
                            checkbox options control.scope schema fieldState

                        -- Html.text "checkbox: unimplemented"
                        _ ->
                            Html.nothing
    in
    Html.viewMaybe
        (\cs ->
            div [ id (Pointer.toString control.scope) ] [ controlBody cs ]
        )
        mControlSchema


schemaView : Options -> Pointer -> Schema -> Form -> Html F.Msg
schemaView options path schema form =
    Html.nothing


type TextFieldType
    = NumberField
    | IntField
    | StringField


isNumericFieldType : TextFieldType -> Bool
isNumericFieldType fieldType =
    List.any ((==) fieldType) [ NumberField, IntField ]


txt : Options -> Pointer -> SubSchema -> F.FieldState -> TextFieldType -> Html F.Msg
txt options path schema f fieldType =
    let
        format : Format
        format =
            schema.format
                |> Maybe.andThen (getFormat options.formats)
                |> Maybe.withDefault
                    { prefix = Nothing
                    , suffix = Nothing
                    , placeholder = Nothing
                    , autocomplete = Nothing
                    , inputType = Nothing
                    , lines = 1
                    , input = Nothing
                    , validation = Form.Validate.succeed
                    }

        attributes : List (Attribute msg)
        attributes =
            [ Attrs.map never <|
                options.theme.txt
                    { withError = f.error /= Nothing
                    , format = schema.format
                    }
            , id f.path
            , Attr.attributeIf (isNumericFieldType fieldType) <| attribute "type" "number"
            , Attr.attributeMaybe placeholder format.placeholder
            , case format.autocomplete of
                Just "on" ->
                    autocomplete True

                Just "off" ->
                    autocomplete False

                Just str ->
                    attribute "autocomplete" str

                Nothing ->
                    autocomplete True
            ]

        textInput : Html F.Msg
        textInput =
            inputGroup
                options.theme
                format.prefix
                format.suffix
                [ if format.lines > 1 then
                    Input.textArea f (attributes ++ [ rows format.lines ])

                  else
                    case format.input of
                        Just html ->
                            html f attributes

                        Nothing ->
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
                            (case fieldType of
                                NumberField ->
                                    Input.floatInput

                                IntField ->
                                    Input.intInput

                                StringField ->
                                    Input.textInput
                            )
                                f
                                (attributes
                                    ++ [ type_
                                            (format.inputType
                                                |> Maybe.withDefault inputType
                                            )
                                       ]
                                )
                ]
    in
    field options
        schema
        f
        [ fieldTitle options.theme schema path |> Maybe.withDefault (text "")
        , textInput
        ]


checkbox : Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
checkbox options path schema f =
    let
        content : List (Html F.Msg)
        content =
            [ div [ Attrs.map never options.theme.checkboxWrapper ]
                [ Input.checkboxInput f
                    [ Attrs.map never <| options.theme.checkboxInput { withError = f.error /= Nothing }
                    , id f.path
                    ]
                , div [ Attrs.map never options.theme.checkboxTitle ]
                    [ fieldTitle options.theme schema path |> Maybe.withDefault (text "") ]
                ]
            ]

        meta : List (Html F.Msg)
        meta =
            Maybe.values [ fieldDescription options.theme schema ]

        feedback : List (Html F.Msg)
        feedback =
            Maybe.values [ error options.theme options.errors f ]
    in
    div
        [ classList
            [ ( "form-group", True )
            , ( "form-check", True )
            , ( "is-invalid", f.error /= Nothing )
            ]
        ]
        [ label [ class "form-check-label" ]
            [ div [ class "field-input" ] (content ++ feedback)
            , case meta of
                [] ->
                    text ""

                html ->
                    div [ class "field-meta" ] html
            ]
        ]



-- TODO: add a None option


select : Options -> Pointer -> SubSchema -> F.FieldState -> TextFieldType -> Html F.Msg
select options path schema f fieldType =
    let
        values : List String
        values =
            Maybe.toList schema.enum |> List.concat |> List.map (Decode.decodeValue UI.decodeStringLike >> Result.withDefault "") |> List.append [ "" ]

        items : List ( String, String )
        items =
            List.map (\v -> ( v, v )) values
    in
    field options
        schema
        f
        [ fieldTitle options.theme schema path |> Maybe.withDefault (text "")
        , (case fieldType of
            StringField ->
                Input.textSelectInput

            NumberField ->
                Input.floatSelectInput

            IntField ->
                Input.intSelectInput
          )
            items
            f
            [ Attrs.map never <| options.theme.select { withError = f.error /= Nothing }
            , id <| Pointer.toString path ++ "-input"
            ]
        ]


option : (SubSchema -> Maybe String) -> Schema -> ( String, Maybe SubSchema )
option attr schema =
    case schema of
        BooleanSchema _ ->
            ( "", Nothing )

        ObjectSchema schema_ ->
            ( attr schema_ |> Maybe.withDefault ""
            , Just schema_
            )


field : Options -> SubSchema -> F.FieldState -> List (Html F.Msg) -> Html F.Msg
field options schema f content =
    let
        description : Html F.Msg
        description =
            Maybe.withDefault Html.nothing <| fieldDescription options.theme schema

        errorMessage : Html F.Msg
        errorMessage =
            Maybe.withDefault Html.nothing <| error options.theme options.errors f
    in
    div
        [ Attrs.map never <|
            options.theme.field
                { withError =
                    f.error /= Nothing
                , withValue =
                    f.value /= Field.Empty
                }
        ]
        [ label [ for f.path, Attrs.map never options.theme.fieldLabel ]
            [ div [ Attrs.map never options.theme.fieldInput ] content
            , div [ Attrs.map never options.theme.fieldInputDescription ] [ description ]
            , errorMessage
            ]
        ]


fieldTitle : Theme -> SubSchema -> Pointer -> Maybe (Html F.Msg)
fieldTitle theme schema path =
    schema.title
        -- If it does not have a title, derive from property name, unCamelCasing it
        |> Maybe.orElse (List.last path |> Maybe.map (String.Case.convertCase " " True True))
        |> Maybe.map (\str -> span [ Attrs.map never theme.fieldTitle ] [ text str ])


fieldDescription : Theme -> SubSchema -> Maybe (Html F.Msg)
fieldDescription theme schema =
    schema.description
        |> Maybe.map (\str -> div [ Attrs.map never theme.fieldDescription ] [ text str ])


error : Theme -> (String -> ErrorValue -> String) -> F.FieldState -> Maybe (Html F.Msg)
error theme func f =
    f.error
        |> Maybe.map
            (\err ->
                div
                    [ Attrs.map never theme.liveError
                    , style "display" "block"
                    ]
                    [ text (func f.path err) ]
            )


inputGroup : Theme -> Maybe String -> Maybe String -> List (Html F.Msg) -> Html F.Msg
inputGroup theme prefix suffix content =
    let
        prepend : List (Html msg)
        prepend =
            case prefix of
                Just string ->
                    [ div
                        [ Attrs.map never theme.inputGroupPrepend ]
                        [ span
                            [ Attrs.map never theme.inputGroupPrependContent ]
                            [ text string ]
                        ]
                    ]

                Nothing ->
                    []

        append : List (Html msg)
        append =
            case suffix of
                Just string ->
                    [ div
                        [ Attrs.map never theme.inputGroupAppend ]
                        [ span
                            [ Attrs.map never theme.inputGroupAppendContent ]
                            [ text string ]
                        ]
                    ]

                Nothing ->
                    []
    in
    div
        [ Attrs.map never theme.inputGroup ]
        (prepend ++ content ++ append)


fieldset : SubSchema -> List (Html F.Msg) -> Html F.Msg
fieldset schema content =
    let
        title : List (Html msg)
        title =
            case schema.title of
                Just str ->
                    [ legend [ class "text-2xl pb-2" ] [ text str ] ]

                Nothing ->
                    []
    in
    Html.fieldset [ tabindex -1 ] (title ++ content)


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click"
        (Decode.succeed <| alwaysPreventDefault msg)


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


conditional : String -> F.FieldState -> List ( String, Html F.Msg ) -> Html F.Msg
conditional className f conditions =
    let
        cond : ( String, b ) -> Maybe ( String, b )
        cond ( value, html ) =
            if Field.valueAsString f.value == value then
                Just ( value, html )

            else
                Nothing
    in
    Html.Keyed.node "div" [ class className ] <|
        List.filterMap cond conditions


getFormat : Dict String Format -> String -> Maybe Format
getFormat formats format =
    Dict.get format formats
