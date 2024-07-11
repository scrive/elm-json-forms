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
            div [] [ Html.h6 [class "my-4"] [text l.text] ]


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
                                intInput options control.scope schema fieldState

                        SingleType NumberType ->
                            if schema.enum /= Nothing then
                                select options control.scope schema fieldState NumberField

                            else
                                numberInput options control.scope schema fieldState

                        SingleType StringType ->
                            if schema.enum /= Nothing then
                                select options control.scope schema fieldState StringField

                            else
                                if (control.options |> Maybe.andThen (.multi)) == Just True then
                                    textarea options control.scope schema fieldState
                                else
                                    textInput options control.scope schema fieldState

                        SingleType BooleanType ->
                            checkbox options control.scope schema fieldState

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


textInput : Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
textInput options path schema fieldState =
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
        path
        schema
        fieldState



intInput : Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
intInput options path schema fieldState =
    fieldGroup (Input.intInput options fieldState []) options
        path
        schema
        fieldState



numberInput : Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
numberInput options path schema fieldState =
    fieldGroup (Input.floatInput options fieldState []) options
        path
        schema
        fieldState


textarea : Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
textarea options path schema state =
    fieldGroup (Input.textArea options state []) options path schema state


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



fieldGroup : Html F.Msg -> Options -> Pointer -> SubSchema -> F.FieldState -> Html F.Msg
fieldGroup inputField options path schema fieldState  =
    let
        description : Html F.Msg
        description =
            Maybe.withDefault Html.nothing <| fieldDescription options.theme schema

        errorMessage : Html F.Msg
        errorMessage =
            Maybe.withDefault Html.nothing <| error options.theme options.errors fieldState
    in
    div
        [ Attrs.map never <|
            options.theme.field
                { withError =
                    fieldState.error /= Nothing
                , withValue =
                    fieldState.value /= Field.Empty
                }
        ]
        [ label [ for fieldState.path, Attrs.map never options.theme.fieldLabel ]
            [ div [ Attrs.map never options.theme.fieldInput ]
                [ fieldTitle options.theme schema path |> Maybe.withDefault (text "")
                , div [ Attrs.map never options.theme.inputGroup ]
                        [inputField]
                ]
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


inputGroup : Theme -> List (Html F.Msg) -> Html F.Msg
inputGroup theme content =
    div [ Attrs.map never theme.inputGroup ] content


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
