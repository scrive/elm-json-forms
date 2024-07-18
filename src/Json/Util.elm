module Json.Util exposing (unSchemata, withObjectSchema)

import Json.Schema.Definitions as Schema exposing (Schemata, Schema (..), SubSchema)

unSchemata : Schemata -> List ( String, Schema )
unSchemata (Schema.Schemata l) =
    l

withObjectSchema : a -> Schema -> (SubSchema -> a) -> a
withObjectSchema boolSchemaResult schema f = case schema of
    BooleanSchema _ -> boolSchemaResult
    ObjectSchema subSchema -> f subSchema
