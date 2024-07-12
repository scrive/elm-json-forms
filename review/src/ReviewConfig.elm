module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoBooleanCase
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoEmptyText
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoLeftPizza
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoRedundantCons
import NoSimpleLetBody
import NoTestValuesInProductionCode
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Modules.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoMissingTypeAnnotation.rule
    , NoRedundantCons.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoEmptyText.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , NoExposingEverything.rule
    , NoTestValuesInProductionCode.rule (NoTestValuesInProductionCode.startsWith "test_")
        |> Rule.filterErrorsForFiles (String.contains "/src/")
    , NoInconsistentAliases.config
        [ ( "Html.Extra", "Html" )
        , ( "Html.Attributes", "Attrs" )
        , ( "Html.Attributes.Extra", "Attrs" )
        , ( "Html.Events", "Events" )
        , ( "Html.Events.Extra", "Events" )
        , ( "List.Extra", "List" )
        , ( "Maybe.Extra", "Maybe" )
        , ( "Http.Extra", "Http" )
        , ( "Json.Encode", "Encode" )
        , ( "Json.Encode.Extra", "Encode" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Decode.Pipeline", "Decode" )
        , ( "Json.Decode.Extra", "Decode" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    ]
