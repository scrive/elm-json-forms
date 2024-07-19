module Main exposing (main)

import Browser
import Form exposing (Form, Msg)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Theme as Theme
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Attributes.Extra as Attrs
import Html.Events as Attrs exposing (onSubmit)
import Examples.Basic exposing (..)
import Json.Encode as Encode
import Json.Schema
import Result.Extra as Result
import Json.Schema.Definitions exposing (Schema)
import List.Extra as List
import UiSchema exposing (UiSchema)
import Html.Extra exposing (viewMaybe)
import Settings exposing (errorString)
import Model exposing (..)
import Examples exposing (exampleForms)

main : Program () MainState MainMsg
main =
    Browser.sandbox { init = {forms = exampleForms, activeForm = 0}, update = update, view = view }


update : MainMsg -> MainState -> MainState
update m state =
  case m of
    ExampleMsg i msg ->
      let
        updateForm : FormState -> FormState
        updateForm fs =
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
      in
        { state
          | forms = List.updateAt i updateForm state.forms
        }
    SwitchTo i -> { state | activeForm = i }


view : MainState -> Html MainMsg
view state =
    div []
      [ aside [ Attrs.class "fixed w-80 top-0 left-0 p-3 bg-green" ]
        [ h1 "Examples"
        , hr [Attrs.class "my-3"] []
        , div []
          <| List.indexedMap viewLink state.forms
        ]
      , Html.map (ExampleMsg state.activeForm) <| viewMaybe viewExample (List.getAt state.activeForm state.forms)
      ]


viewLink : Int -> FormState -> Html MainMsg
viewLink i fs =
    div [class "my-1 cursor-pointer"]
      [ a [Attrs.class "hover:underline text-blue-600", Attrs.onClick (SwitchTo i)] [text fs.title]]

viewExample : FormState -> Html ExampleMsg
viewExample fs =
  div [Attrs.class "p-3 sm:ml-80"]
    [ div []
        [ h1 fs.title
        , hr [] []
        ]
    , div [ Attrs.class "flex flex-wrap -mx-2" ]
      [
        div [Attrs.class "w-1/2 px-2"]
          [ div []
            [ h2 "Form"
            , case fs.form of
              Nothing -> empty
              Just form ->
                div [class "border border-black p-3 bg-gray-50"]
                  [ Html.map FormMsg (Form.view form)]
            ]
          , div []
          [ h2 "Data"
          , case fs.form of
            Nothing -> empty
            Just form -> viewData form
          ]
        ]
      , div [Attrs.class "w-1/2 px-2"]
        [ div []
          [ h2 "JSON Schema"
          , textarea (Attrs.onInput <| EditSchema) fs.stringSchema
          , viewMaybe viewError fs.schemaError
          ]
        , div []
          [ h2 "UI Schema"
          , case fs.stringUiSchema of
              Just stringUiSchema -> textarea (Attrs.onInput <| EditUiSchema) stringUiSchema
              Nothing -> empty
          , viewMaybe viewError fs.uiSchemaError
          ]
        ]
      ]
    ]

empty : Html a
empty = Html.span [class "text-gray-500"] [text "(empty)"]

viewError : String -> Html a
viewError err =
  div []
            [ h3 [class "text-red-600 font-bold mt-1"] [text "Error:"]
            , pre [class "text-sm overflow-scroll" ] [text err]
            ]

viewData : Form -> Html a
viewData form =
  let
      nErrors =
            List.length <| Error.getErrors form.state.errors

      dataText =
          Encode.encode 4 form.state.value
  in
    div []
    [ textarea (Attrs.readonly True) dataText
    , if nErrors == 0 then
          text "No errors."

        else
          text <| String.fromInt nErrors ++ " errors."
    ]


textarea : (Attribute a) -> String -> Html a
textarea attr s = Html.textarea
  [ Attrs.rows 15
  , Attrs.class "text-sm block w-full font-mono bg-gray-50"
  , Attrs.spellcheck False
  , attr
  ] [ text s ]



h1 : String -> Html a
h1 t = Html.h1 [Attrs.class "text-3xl my-5"] [ text t ]

h2 : String -> Html a
h2 t = Html.h2 [ Attrs.class "text-xl pt-4 pb-2" ] [ text t ]