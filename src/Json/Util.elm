module Json.Util exposing (unSchemata, getProperties, withObjectSchema)

import Json.Schema.Definitions as Schema exposing (Schemata, Schema (..), SubSchema)

unSchemata : Schemata -> List ( String, Schema )
unSchemata (Schema.Schemata l) =
    l

getProperties : SubSchema -> List (String, Schema)
getProperties o = Maybe.withDefault [] <| Maybe.map unSchemata o.properties

withObjectSchema : a -> Schema -> (SubSchema -> a) -> a
withObjectSchema boolSchemaResult schema f = case schema of
    BooleanSchema _ -> boolSchemaResult
    ObjectSchema subSchema -> f subSchema
