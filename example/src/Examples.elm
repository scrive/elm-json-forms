module Examples exposing (exampleForms)

import Examples.Basic exposing (..)
import Examples.Categorization1 exposing (..)
import Examples.Control1 exposing (..)
import Examples.Control2 exposing (..)
import Examples.GenerateUiSchema exposing (..)
import Examples.Layout exposing (..)
import Examples.LayoutGroup exposing (..)
import Examples.LayoutHorizontal exposing (..)
import Examples.LayoutNested exposing (..)
import Examples.LayoutVertical exposing (..)
import Examples.Rule exposing (..)
import Model exposing (FormState, MainState, makeForm)


exampleForms : List FormState
exampleForms =
    [ makeForm "Basic Example" basicExampleSchema (Just basicExampleUiSchema)
    , makeForm "Control Example 1" controlExample1Schema (Just controlExample1UiSchema)
    , makeForm "Control Example 2" controlExample2Schema (Just controlExample2UiSchema)
    , makeForm "Categorization Example 1" categorizationExample1Schema (Just categorizationExample1UiSchema)
    , makeForm "Rule Example" ruleExampleSchema (Just ruleExampleUiSchema)
    , makeForm "Layout Example: Horizontal layout" layoutExampleSchema (Just layoutExample1UiSchema)
    , makeForm "Layout Example: Vertical layout" layoutExampleSchema (Just layoutExample2UiSchema)
    , makeForm "Layout Example: Group" layoutExampleSchema (Just layoutExample3UiSchema)
    , makeForm "Layout Example: Nested layouts" layoutExampleSchema (Just layoutExample4UiSchema)
    , makeForm "Generate UI Schema Example" generateUiSchemaExampleSchema Nothing
    ]
