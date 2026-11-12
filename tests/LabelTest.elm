module LabelTest exposing (suite)

import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import UiSchema.Internal as UI exposing (LabelEmphasis(..), UiSchema(..))


decodeLabel : String -> Result Decode.Error UiSchema
decodeLabel =
    Decode.decodeString UI.decodeUiSchema


suite : Test
suite =
    describe "Label"
        [ test "without options" <|
            \_ ->
                decodeLabel """{ "type": "Label", "text": "Hello" }"""
                    |> Expect.equal (Ok (UiLabel { text = "Hello", options = Nothing }))
        , test "normal emphasis" <|
            \_ ->
                decodeLabel """{ "type": "Label", "text": "Hello", "options": { "emphasis": "normal" } }"""
                    |> Expect.equal (Ok (UiLabel { text = "Hello", options = Just { emphasis = Just EmphasisNormal } }))
        , test "bold emphasis" <|
            \_ ->
                decodeLabel """{ "type": "Label", "text": "Hello", "options": { "emphasis": "bold" } }"""
                    |> Expect.equal (Ok (UiLabel { text = "Hello", options = Just { emphasis = Just EmphasisBold } }))
        , test "unknown emphasis falls back to the default" <|
            \_ ->
                decodeLabel """{ "type": "Label", "text": "Hello", "options": { "emphasis": "italic" } }"""
                    |> Expect.equal (Ok (UiLabel { text = "Hello", options = Just { emphasis = Nothing } }))
        , test "label inside a layout still decodes with options" <|
            \_ ->
                decodeLabel """{ "type": "VerticalLayout", "elements": [ { "type": "Label", "text": "Body", "options": { "emphasis": "normal" } } ] }"""
                    |> Expect.equal
                        (Ok
                            (UiVerticalLayout
                                { elements = [ UiLabel { text = "Body", options = Just { emphasis = Just EmphasisNormal } } ]
                                , rule = Nothing
                                }
                            )
                        )
        ]
