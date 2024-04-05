module Json.Schema.Form.Fields exposing (Options, schemaView)

import Dict exposing (Dict)
import Form as F
import Form.Input as Input
import Form.Validate
import Html exposing (Attribute, Html, button, div, label, legend, li, ol, p, span, text)
import Html.Attributes
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
import Json.Decode
import Json.Schema.Definitions
    exposing
        ( Items(..)
        , Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        , blankSchema
        )
import Json.Schema.Form.Error exposing (ErrorValue, Errors)
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Value exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Case


type alias Options =
    { errors : Errors
    , formats : Dict String Format
    }


type alias Form =
    F.Form ErrorValue Value


type alias Path =
    List String


schemaView : Options -> Path -> Schema -> Form -> Html F.Msg
schemaView options path schema form =
    case schema of
        BooleanSchema value ->
            div []
                [ if value then
                    text "True"

                  else
                    text "False"
                ]

        ObjectSchema subSchema ->
            objectView options path subSchema form


objectView : Options -> Path -> SubSchema -> Form -> Html F.Msg
objectView options path schema form =
    case schema.type_ of
        AnyType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                switch options path schema form

            else
                fieldView options path schema BooleanType form

        NullableType singleType ->
            fieldView options path schema singleType form

        UnionType _ ->
            fieldView options path schema StringType form

        SingleType singleType ->
            fieldView options path schema singleType form


fieldView : Options -> Path -> SubSchema -> SingleType -> Form -> Html F.Msg
fieldView options path schema type_ form =
    case type_ of
        IntegerType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                select options path schema (getFieldAsString path form)

            else
                txt options path schema (getFieldAsString path form) { isNumber = True }

        NumberType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                select options path schema (getFieldAsString path form)

            else
                txt options path schema (getFieldAsString path form) { isNumber = True }

        StringType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                select options path schema (getFieldAsString path form)

            else
                txt options path schema (getFieldAsString path form) { isNumber = False }

        BooleanType ->
            checkbox options path schema (getFieldAsBool path form)

        ArrayType ->
            let
                f : F.FieldState ErrorValue String
                f =
                    getFieldAsString path form
            in
            case schema.items of
                NoItems ->
                    field options schema f <|
                        list options path form ( schema.title, blankSchema )

                ItemDefinition item ->
                    field options schema f <|
                        list options path form ( schema.title, item )

                ArrayOfItems items ->
                    field options schema f <|
                        tuple options path form ( schema.title, items )

        ObjectType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                switch options path schema form

            else
                fieldset schema [ group options path schema form ]

        NullType ->
            div [] []


txt : Options -> Path -> SubSchema -> F.FieldState ErrorValue String -> { isNumber : Bool } -> Html F.Msg
txt options path schema f { isNumber } =
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

        classes : List ( String, Bool )
        classes =
            [ ( "form-control", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , case schema.format of
                Just str ->
                    ( "format-" ++ str, True )

                Nothing ->
                    ( "", False )
            ]

        attributes : List (Attribute msg)
        attributes =
            [ classList classes
            , id f.path
            , Attr.attributeIf isNumber <| attribute "type" "number"
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
                            Input.textInput f
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
        [ fieldTitle schema path |> Maybe.withDefault (text "")
        , textInput
        ]


checkbox : Options -> Path -> SubSchema -> F.FieldState ErrorValue Bool -> Html F.Msg
checkbox options path schema f =
    let
        content : List (Html F.Msg)
        content =
            [ div [ class "checkbox" ]
                [ Input.checkboxInput f
                    [ classList
                        [ ( "form-check-input", True )
                        , ( "is-invalid", f.liveError /= Nothing )
                        ]
                    , id f.path
                    ]
                , fieldTitle schema path |> Maybe.withDefault (text "")
                ]
            ]

        meta : List (Html F.Msg)
        meta =
            Maybe.values [ fieldDescription schema ]

        feedback : List (Html F.Msg)
        feedback =
            Maybe.values [ liveError options.errors f ]
    in
    div
        [ classList
            [ ( "form-group", True )
            , ( "form-check", True )
            , ( "is-invalid", f.liveError /= Nothing )
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


select : Options -> Path -> SubSchema -> F.FieldState ErrorValue String -> Html F.Msg
select options path schema f =
    let
        schemata : List Schema
        schemata =
            List.concat
                [ schema.oneOf |> Maybe.withDefault []
                , schema.anyOf |> Maybe.withDefault []
                ]

        items : List ( String, String )
        items =
            schemata
                |> List.map (option constAsString)
                |> List.map
                    (\( name, schema_ ) ->
                        ( name
                        , schema_
                            |> Maybe.andThen .title
                            |> Maybe.withDefault name
                        )
                    )

        descriptions : List ( String, Html F.Msg )
        descriptions =
            schemata
                |> List.map (option constAsString)
                |> List.map
                    (\( name, schema_ ) ->
                        ( name
                        , schema_
                            |> Maybe.andThen fieldDescription
                            |> Maybe.withDefault (text "")
                        )
                    )
    in
    field options
        schema
        f
        [ fieldTitle schema path |> Maybe.withDefault (text "")
        , Input.selectInput
            items
            f
            [ classList
                [ ( "form-control custom-select", True )
                , ( "is-invalid", f.liveError /= Nothing )
                ]
            , id f.path
            ]
        , conditional "select-more" f descriptions
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


list :
    Options
    -> Path
    -> Form
    -> ( Maybe String, Schema )
    -> List (Html F.Msg)
list options path form ( title, schema ) =
    let
        indexes : List Int
        indexes =
            getListIndexes path form

        itemPath : Int -> Path
        itemPath idx =
            path ++ [ String.fromInt idx ]

        itemView : Int -> Html F.Msg
        itemView idx =
            li
                [ class "list-group-item" ]
                [ schemaView options (itemPath idx) schema form
                , button
                    [ onClickPreventDefault (F.RemoveItem (fieldPath path) idx)
                    , class "btn btn-outline-secondary btn-sm btn-remove"
                    ]
                    [ text "Remove" ]
                ]
    in
    [ ol [ class "list-group mb-2" ] (List.map itemView indexes)
    , button
        [ class "btn btn-secondary btn-add"
        , onClickPreventDefault (F.Append (fieldPath path))
        ]
        [ text (title |> Maybe.withDefault "Add")
        ]
    ]


tuple :
    Options
    -> Path
    -> Form
    -> ( Maybe String, List Schema )
    -> List (Html F.Msg)
tuple options path form ( title, schemata ) =
    let
        itemPath : Int -> Path
        itemPath idx =
            path ++ [ "tuple" ++ String.fromInt idx ]

        itemView : Int -> Schema -> Html F.Msg
        itemView idx itemSchema =
            div
                [ class "col" ]
                [ schemaView options (itemPath idx) itemSchema form ]
    in
    [ case title of
        Just str ->
            div [ class "field-title" ] [ text str ]

        Nothing ->
            text ""
    , div [ class "form-row" ] (List.indexedMap itemView schemata)
    ]


radio : F.FieldState ErrorValue String -> ( String, String ) -> Html F.Msg
radio fieldState ( value, title ) =
    label [ class "form-check-label" ]
        [ Input.radioInput value
            fieldState
            [ class "form-check-input"
            , id (fieldPath [ fieldState.path, value ])
            ]
        , span [ class "label-text" ] [ text title ]
        ]


switch : Options -> Path -> SubSchema -> Form -> Html F.Msg
switch options path schema form =
    let
        f : F.FieldState ErrorValue String
        f =
            getFieldAsString (path ++ [ "switch" ]) form

        schemata : List Schema
        schemata =
            List.concat
                [ schema.oneOf |> Maybe.withDefault []
                , schema.anyOf |> Maybe.withDefault []
                ]

        items : List ( String, Maybe SubSchema )
        items =
            schemata
                |> List.map (option .title)

        itemId : Int -> String
        itemId idx =
            "option" ++ String.fromInt idx

        itemButton : Int -> ( String, b ) -> Html F.Msg
        itemButton idx ( title, _ ) =
            div
                [ classList
                    [ ( "form-check", True )
                    , ( "form-check-inline", List.length items <= 2 )
                    ]
                ]
                [ radio f ( itemId idx, title ) ]

        itemFields : Int -> ( String, Maybe SubSchema ) -> ( String, Html F.Msg )
        itemFields idx ( _, schema_ ) =
            case schema_ of
                Just s ->
                    ( itemId idx
                    , case s.const of
                        Just _ ->
                            text ""

                        Nothing ->
                            objectView options (path ++ [ "value" ]) s form
                    )

                Nothing ->
                    ( itemId idx, text "" )
    in
    field options schema f <|
        [ fieldTitle schema path |> Maybe.withDefault (text "")
        , div [ class "switch", id f.path, tabindex -1 ]
            (List.indexedMap itemButton items)
        , conditional "switch-more" f (List.indexedMap itemFields items)
        ]


field : Options -> SubSchema -> F.FieldState ErrorValue String -> List (Html F.Msg) -> Html F.Msg
field options schema f content =
    let
        meta : List (Html F.Msg)
        meta =
            Maybe.values [ fieldDescription schema ]

        feedback : List (Html F.Msg)
        feedback =
            Maybe.values [ liveError options.errors f ]
    in
    div
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , ( "has-value", f.value /= Nothing && f.value /= Just "" )
            ]
        ]
        [ label [ for f.path, class "d-block" ]
            [ div [ class "field-input" ] (content ++ feedback)
            , case meta of
                [] ->
                    text ""

                html ->
                    div [ class "field-meta" ] html
            ]
        ]


group : Options -> Path -> SubSchema -> Form -> Html F.Msg
group options path schema form =
    let
        f : F.FieldState ErrorValue String
        f =
            getFieldAsString path form

        schemataItem : ( String, Schema ) -> Html F.Msg
        schemataItem ( name, subSchema ) =
            schemaView options (path ++ [ name ]) subSchema form

        fields : List (Html F.Msg)
        fields =
            case schema.properties of
                Nothing ->
                    []

                Just (Json.Schema.Definitions.Schemata schemata) ->
                    List.map schemataItem schemata

        meta : List (Html msg)
        meta =
            schema.description |> Html.viewMaybe (\str -> p [] [ text str ]) |> List.singleton

        feedback : List (Html F.Msg)
        feedback =
            Maybe.values [ liveError options.errors f ]
    in
    div
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , ( "has-value", f.value /= Nothing && f.value /= Just "" )
            ]
        ]
        (meta ++ fields ++ feedback)


fieldTitle : SubSchema -> Path -> Maybe (Html F.Msg)
fieldTitle schema path =
    schema.title
        -- If it does not have a title, derive from property name, unCamelCasing it
        |> Maybe.orElse (List.last path |> Maybe.map (String.Case.convertCase " " True True))
        |> Maybe.map (\str -> span [ class "label-text" ] [ text str ])


fieldDescription : SubSchema -> Maybe (Html F.Msg)
fieldDescription schema =
    schema.description
        |> Maybe.map (\str -> div [ class "form-text text-muted" ] [ text str ])


liveError : Errors -> F.FieldState ErrorValue a -> Maybe (Html F.Msg)
liveError func f =
    f.liveError
        |> Maybe.map
            (\err ->
                div
                    [ class "invalid-feedback"
                    , style "display" "block"
                    ]
                    [ text (func f.path err) ]
            )


inputGroup : Maybe String -> Maybe String -> List (Html F.Msg) -> Html F.Msg
inputGroup prefix suffix content =
    let
        prepend : List (Html msg)
        prepend =
            case prefix of
                Just string ->
                    [ div
                        [ class "input-group-prepend" ]
                        [ div
                            [ class "input-group-text" ]
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
                        [ class "input-group-append" ]
                        [ div
                            [ class "input-group-text" ]
                            [ text string ]
                        ]
                    ]

                Nothing ->
                    []
    in
    div
        [ class "input-group" ]
        (prepend ++ content ++ append)


fieldset : SubSchema -> List (Html F.Msg) -> Html F.Msg
fieldset schema content =
    let
        title : List (Html msg)
        title =
            case schema.title of
                Just str ->
                    [ legend [] [ text str ] ]

                Nothing ->
                    []
    in
    Html.fieldset [ tabindex -1 ] (title ++ content)


getFieldAsBool : Path -> F.Form e o -> F.FieldState e Bool
getFieldAsBool path =
    F.getFieldAsBool (fieldPath path)


getFieldAsString : Path -> F.Form e o -> F.FieldState e String
getFieldAsString path =
    F.getFieldAsString (fieldPath path)


getListIndexes : Path -> F.Form e o -> List Int
getListIndexes path =
    F.getListIndexes (fieldPath path)


fieldPath : Path -> String
fieldPath =
    String.join "."


constAsString : SubSchema -> Maybe String
constAsString schema =
    let
        decoder : Json.Decode.Decoder String
        decoder =
            Json.Decode.oneOf
                [ Json.Decode.string
                , Json.Decode.int |> Json.Decode.map String.fromInt
                , Json.Decode.float |> Json.Decode.map String.fromFloat
                ]
    in
    schema.const
        |> Maybe.map (Json.Decode.decodeValue decoder)
        |> Maybe.andThen Result.toMaybe


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click"
        (Json.Decode.succeed <| alwaysPreventDefault msg)


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


conditional : String -> F.FieldState e String -> List ( String, Html F.Msg ) -> Html F.Msg
conditional className f conditions =
    let
        cond : ( String, b ) -> Maybe ( String, b )
        cond ( value, html ) =
            if f.value == Just value then
                Just ( value, html )

            else
                Nothing
    in
    Html.Keyed.node "div" [ class className ] <|
        List.filterMap cond conditions


getFormat : Dict String Format -> String -> Maybe Format
getFormat formats format =
    Dict.get format formats
