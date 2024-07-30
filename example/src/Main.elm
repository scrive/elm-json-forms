module Main exposing (main)

import Browser
import Examples exposing (exampleForms)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Attributes.Extra as Attrs
import Html.Events as Events
import Html.Extra as Html exposing (viewMaybe)
import Json.Encode as Encode
import Json.Pointer as Pointer
import Json.Schema
import List.Extra as List
import Model exposing (..)
import Settings
import UiSchema


main : Program () MainState MainMsg
main =
    Browser.sandbox { init = { forms = exampleForms, activeForm = 0 }, update = update, view = view }


update : MainMsg -> MainState -> MainState
update m state =
    case m of
        ExampleMsg i msg ->
            { state
                | forms = List.updateAt i (updateExample msg) state.forms
            }

        SwitchTo i ->
            { state | activeForm = i }


updateExample : ExampleMsg -> FormState -> FormState
updateExample msg fs =
    case msg of
        FormMsg formMsg ->
            { fs | form = Maybe.map (Form.update formMsg) fs.form }

        EditSchema s ->
            case Json.Schema.fromString s of
                Ok schema ->
                    { fs
                        | stringSchema = s
                        , form = Maybe.map (Form.setSchema schema) fs.form
                        , schemaError = Nothing
                    }

                Err e ->
                    { fs
                        | stringSchema = s
                        , schemaError = Just e
                    }

        EditUiSchema s ->
            case UiSchema.fromString s of
                Ok uiSchema ->
                    { fs
                        | stringUiSchema = Just s
                        , form = Maybe.map (Form.setUiSchema (Just uiSchema)) fs.form
                        , uiSchemaError = Nothing
                    }

                Err e ->
                    { fs
                        | stringUiSchema = Just s
                        , uiSchemaError = Just e
                    }

        SwitchTab t ->
            { fs
                | tab = t
            }


view : MainState -> Html MainMsg
view state =
    div [class "flex flex-wrap"]
        [ viewMenu state
        , Html.map (ExampleMsg state.activeForm) <| viewMaybe viewExample (List.getAt state.activeForm state.forms)
        ]


viewMenu : MainState -> Html MainMsg
viewMenu state =
    aside [ class "md:w-1/4 p-3" ]
        [ h1 "Examples"
        , hr [ class "my-3" ] []
        , div [] <|
            List.indexedMap (viewLink state.activeForm) state.forms
        , hr [ class "my-3" ] []
        , viewGithubIcon
        ]


viewLink : Int -> Int -> FormState -> Html MainMsg
viewLink activeId linkId fs =
    div
        [ Attrs.class "my-1 cursor-pointer"
        ]
        [ a
            [ Attrs.classList
                [ ( "hover:underline text-blue-600", activeId /= linkId )
                , ( "font-bold", activeId == linkId )
                ]
            , Events.onClick (SwitchTo linkId)
            ]
            [ text fs.title ]
        ]


viewExample : FormState -> Html ExampleMsg
viewExample fs =
    div [ class "p-3 w-full md:w-3/4" ]
        [ div []
            [ h1 fs.title
            , hr [] []
            ]
        , div [ class "flex flex-wrap -mx-2" ]
            [ div [ class "w-full lg:w-1/2 px-2" ]
                [ div []
                    [ h2 "Form"
                    , case fs.form of
                        Nothing ->
                            empty

                        Just form ->
                            div [ class "border shadow rounded p-3" ]
                                [ Html.map FormMsg (Form.view form) ]
                    ]
                ]
            , div [ class "w-full lg:w-1/2 px-2" ]
                [ div [ class "border-b mb-3" ]
                    [ viewTabHeader [] fs.tab DataTab
                    , viewTabHeader
                        [ if fs.schemaError /= Nothing then
                            class "line-through"

                          else
                            Attrs.empty
                        ]
                        fs.tab
                        JsonSchemaTab
                    , viewTabHeader
                        [ if fs.uiSchemaError /= Nothing then
                            class "line-through"

                          else
                            Attrs.empty
                        ]
                        fs.tab
                        UiSchemaTab
                    ]
                , div []
                    [ div [ Attrs.hidden (fs.tab /= DataTab) ]
                        [ Html.viewMaybe viewData fs.form
                        ]
                    , div [ Attrs.hidden (fs.tab /= JsonSchemaTab) ]
                        [ textarea [ Attrs.name "JsonSchema", Events.onInput EditSchema, Attrs.rows 30 ] fs.stringSchema
                        , viewMaybe (viewError "Error") fs.schemaError
                        ]
                    , div [ Attrs.hidden (fs.tab /= UiSchemaTab) ]
                        [ case fs.stringUiSchema of
                            Just stringUiSchema ->
                                textarea [ Attrs.name "UiSchema", Events.onInput EditUiSchema, Attrs.rows 30 ] stringUiSchema

                            Nothing ->
                                empty
                        , viewMaybe (viewError "Error") fs.uiSchemaError
                        ]
                    ]
                ]
            ]
        ]


viewGithubIcon : Html a
viewGithubIcon =
    Html.a [ Attrs.href "https://github.com/scrive/elm-json-forms" ] [ Html.img [ Attrs.src "https://github.githubassets.com/assets/GitHub-Mark-ea2971cee799.png", Attrs.width 40 ] [] ]


viewTabHeader : List (Attribute ExampleMsg) -> Tab -> Tab -> Html ExampleMsg
viewTabHeader attrs activeTab tab =
    button
        ([ Attrs.classList
            [ ( "p-4 pb-2", True )
            , ( "text-blue-500 border-blue-500 border-b-2", tab == activeTab )
            ]
         , Events.onClick (SwitchTab tab)
         ]
            ++ attrs
        )
        [ text <|
            case tab of
                DataTab ->
                    "Data"

                JsonSchemaTab ->
                    "JSON Schema"

                UiSchemaTab ->
                    "UI Schema"
        ]


empty : Html a
empty =
    Html.span [ class "text-gray-500" ] [ text "(empty)" ]


viewError : String -> String -> Html a
viewError title err =
    div []
        [ h3 [ class "text-red-600 font-bold mt-1" ] [ text <| title ++ ":" ]
        , pre [ class "text-sm overflow-scroll" ] [ text err ]
        ]


viewData : Form -> Html a
viewData form =
    let
        dataText =
            Encode.encode 4 form.state.value

        errorsText =
            String.join "\n" (List.map (\( pointer, err ) -> Pointer.toString pointer ++ ": " ++ Settings.errorString err) <| Form.getErrors form)
    in
    div []
        [ textarea [ Attrs.id "Data", Attrs.readonly True, Attrs.rows 15 ] dataText
        , case Form.getErrors form of
            [] ->
                Html.nothing

            _ ->
                viewError "Errors" errorsText
        ]


textarea : List (Attribute a) -> String -> Html a
textarea attrs s =
    Html.textarea
        ([ class "text-sm block w-full font-mono border border-gray-200 shadow rounded p-3"
         , Attrs.spellcheck False
         ]
            ++ attrs
        )
        [ text s ]


h1 : String -> Html a
h1 t =
    Html.h1 [ class "text-3xl my-5" ] [ text t ]


h2 : String -> Html a
h2 t =
    Html.h2 [ class "text-xl p-4 pb-2 mb-3" ] [ text t ]
