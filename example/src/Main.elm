module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cmd.Extra as Cmd
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
import Maybe.Extra as Maybe
import Model exposing (..)
import UiSchema
import Url
import Url.Parser
import Url.Parser.Query


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        activeForm =
            Maybe.withDefault 0 <| parseUrl url
    in
    ( { forms = exampleForms, activeForm = activeForm, key = key }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExampleMsg i m ->
            case List.getAt i model.forms |> Maybe.map (updateExample m) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( st, cmd ) ->
                    ( { model
                        | forms = List.updateAt i (always st) model.forms
                      }
                    , Cmd.map (ExampleMsg i) cmd
                    )

        UrlChanged url ->
            case parseUrl url of
                Just eid ->
                    ( { model | activeForm = eid }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


makeUrl : Int -> String
makeUrl exampleId =
    "?example=" ++ String.fromInt exampleId


parseUrl : Url.Url -> Maybe Int
parseUrl url =
    Maybe.join <| Url.Parser.parse (Url.Parser.query <| Url.Parser.Query.int "example") { url | path = "" }


updateExample : ExampleMsg -> FormState -> ( FormState, Cmd ExampleMsg )
updateExample msg fs =
    case msg of
        FormMsg formMsg ->
            ( { fs | form = Maybe.map (Form.update formMsg) fs.form }, Cmd.none )

        EditSchema s ->
            case Json.Schema.fromString s of
                Ok schema ->
                    ( { fs
                        | stringSchema = s
                        , form = Maybe.map (Form.setSchema schema) fs.form
                        , schemaError = Nothing
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { fs
                        | stringSchema = s
                        , schemaError = Just e
                      }
                    , Cmd.none
                    )

        Submit ->
            ( fs, Cmd.perform (FormMsg Form.validateAllFieldsMsg) )

        EditUiSchema s ->
            case UiSchema.fromString s of
                Ok uiSchema ->
                    ( { fs
                        | stringUiSchema = Just s
                        , form = Maybe.map (Form.setUiSchema (Just uiSchema)) fs.form
                        , uiSchemaError = Nothing
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { fs
                        | stringUiSchema = Just s
                        , uiSchemaError = Just e
                      }
                    , Cmd.none
                    )

        SwitchTab t ->
            ( { fs | tab = t }, Cmd.none )


view : Model -> Browser.Document Msg
view state =
    { title = "JSON Schema Forms"
    , body =
        [ div [ class "flex flex-wrap bg-slate-100 h-lvh" ]
            [ viewMenu state
            , Html.map (ExampleMsg state.activeForm) <| viewMaybe viewExample (List.getAt state.activeForm state.forms)
            ]
        ]
    }


viewMenu : Model -> Html Msg
viewMenu state =
    aside [ class "md:w-1/4 max-w-80 p-3" ]
        [ h1 "Examples"
        , hr [ class "my-3" ] []
        , div [] <|
            List.indexedMap (viewLink state.activeForm) state.forms
        , hr [ class "my-3" ] []
        , viewGithubIcon
        ]


viewLink : Int -> Int -> FormState -> Html Msg
viewLink activeId linkId fs =
    div
        [ Attrs.class "my-1 cursor-pointer"
        ]
        [ a
            [ Attrs.classList
                [ ( "hover:underline text-blue-600", activeId /= linkId )
                , ( "font-bold", activeId == linkId )
                ]
            , Attrs.href <| makeUrl linkId
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
                            div [ class "border shadow rounded p-3 bg-white" ]
                                [ Html.map FormMsg (Form.viewWidget (Form.widget form))
                                , button
                                    [ Attrs.class "bg-blue-500 text-white p-2 rounded"
                                    , Events.onClick Submit
                                    ]
                                    [ text "Validate" ]
                                ]
                    ]
                ]
            , div [ class "w-full lg:w-1/2 px-2" ]
                [ div [ class "border-b mb-3" ]
                    [ viewTabHeader [] fs.tab RawDataTab
                    , viewTabHeader [] fs.tab SubmitDataTab
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
                    [ div [ Attrs.hidden (fs.tab /= RawDataTab) ]
                        [ Html.viewMaybe viewRawData fs.form
                        ]
                    , div [ Attrs.hidden (fs.tab /= SubmitDataTab) ]
                        [ Html.viewMaybe viewSubmitData fs.form
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
                RawDataTab ->
                    "Raw Data"

                SubmitDataTab ->
                    "Submit Data"

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


viewRawData : Form -> Html a
viewRawData form =
    viewData form <| Encode.encode 4 <| Form.getRawValue form


viewSubmitData : Form -> Html a
viewSubmitData form =
    viewData form <|
        case Form.getSubmitValue form of
            Nothing ->
                "<empty>"

            Just v ->
                Encode.encode 4 v


viewData : Form -> String -> Html a
viewData form dataText =
    let
        errorsText =
            String.join "\n" (List.map (\( pointer, err ) -> Pointer.toString pointer ++ ": " ++ Form.errorString err) <| Form.getErrors form)
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
         , Attrs.value s
         ]
            ++ attrs
        )
        []


h1 : String -> Html a
h1 t =
    Html.h1 [ class "text-3xl my-5" ] [ text t ]


h2 : String -> Html a
h2 t =
    Html.h2 [ class "text-xl p-4 pb-2 mb-3" ] [ text t ]
