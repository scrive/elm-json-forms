module Form.FieldValue exposing
    ( FieldType(..)
    , FieldValue(..)
    , asBool
    , asString
    , fromFieldInput
    , pointedFieldValue
    , updateValue
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Json.Pointer as Pointer exposing (Pointer)
import String


type FieldValue
    = String String
    | Int Int
    | Number Float
    | Bool Bool


{-| Types that may be produced by a HTML field
-}
type FieldType
    = NumberField
    | IntField
    | StringField


asString : FieldValue -> String
asString fv =
    case fv of
        String s ->
            s

        Int i ->
            String.fromInt i

        Number n ->
            String.fromFloat n

        Bool True ->
            "True"

        Bool False ->
            "False"


asValue : FieldValue -> Value
asValue fv =
    case fv of
        String s ->
            Encode.string s

        Int i ->
            Encode.int i

        Number n ->
            Encode.float n

        Bool b ->
            Encode.bool b


asBool : FieldValue -> Maybe Bool
asBool fv =
    case fv of
        Bool b ->
            Just b

        _ ->
            Nothing


toFieldValue : Value -> Maybe FieldValue
toFieldValue value =
    case
        Decode.decodeValue
            (Decode.oneOf
                [ Decode.map Int Decode.int
                , Decode.map Number Decode.float
                , Decode.map String Decode.string
                , Decode.map Bool Decode.bool
                ]
            )
            value
    of
        Ok fv ->
            Just fv

        Err _ ->
            Nothing


pointedFieldValue : Pointer -> Value -> Maybe FieldValue
pointedFieldValue pointer value =
    Maybe.andThen toFieldValue (Pointer.pointedValue pointer value)


updateValue : Pointer -> FieldValue -> Value -> Value
updateValue pointer new value =
    case pointer of
        "properties" :: key :: [] ->
            Encode.dict identity identity <|
                case Decode.decodeValue (Decode.dict Decode.value) value of
                    Ok o ->
                        Dict.insert key (asValue new) o

                    Err _ ->
                        Dict.singleton key (asValue new)

        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        Dict.insert key (updateValue ps new (Maybe.withDefault Encode.null <| Dict.get key o)) o

                Err _ ->
                    Encode.dict identity identity <| Dict.singleton key (updateValue ps new Encode.null)

        [] ->
            asValue new

        _ ->
            value


fromIntInput : String -> FieldValue
fromIntInput s =
    Maybe.withDefault (String s) <| Maybe.map Int <| String.toInt s


fromFloatInput : String -> FieldValue
fromFloatInput s =
    Maybe.withDefault (String s) <| Maybe.map Number <| String.toFloat s


fromStringInput : String -> FieldValue
fromStringInput s =
    String s


fromFieldInput : FieldType -> String -> FieldValue
fromFieldInput fieldType =
    case fieldType of
        StringField ->
            fromStringInput

        IntField ->
            fromIntInput

        NumberField ->
            fromFloatInput
