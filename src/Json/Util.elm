module Json.Util exposing (getProperties, unSchemata, withObjectSchema)

import Json.Schema.Definitions as Schema exposing (Schema(..), Schemata, SubSchema)


unSchemata : Schemata -> List ( String, Schema )
unSchemata (Schema.Schemata l) =
    l


getProperties : SubSchema -> List ( String, Schema )
getProperties o =
    Maybe.withDefault [] <| Maybe.map unSchemata o.properties


withObjectSchema : a -> Schema -> (SubSchema -> a) -> a
withObjectSchema boolSchemaResult schema f =
    case schema of
        BooleanSchema _ ->
            boolSchemaResult

        ObjectSchema subSchema ->
            f subSchema
