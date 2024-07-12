module Form.FieldValue exposing
    ( FieldValue(..)
    , asBool
    , asString
    , pointedFieldValue
    , updateValue
    )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Json.Pointer as Pointer exposing (Pointer)
import String
import Dict

type FieldValue
    = String String
    | Int Int
    | Number Float
    | Bool Bool
    | Empty


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

        Empty ->
            ""


asValue : FieldValue -> Maybe Value
asValue fv =
    case fv of
        String s ->
            Just <| Encode.string s

        Int i ->
            Just <| Encode.int i

        Number n ->
            Just <| Encode.float n

        Bool b ->
            Just <| Encode.bool b

        Empty ->
            Nothing


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
                case ( Decode.decodeValue (Decode.dict Decode.value) value, asValue new ) of
                    ( Ok o, Nothing ) ->
                        Dict.remove key o

                    ( Ok o, Just v ) ->
                        Dict.insert key v o

                    ( Err _, Nothing ) ->
                        Dict.empty

                    ( Err _, Just v ) ->
                        Dict.singleton key v

        "properties" :: key :: ps ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok o ->
                    Encode.dict identity identity <|
                        Dict.insert key (updateValue ps new (Maybe.withDefault Encode.null <| Dict.get key o)) o

                Err _ ->
                    Encode.dict identity identity <| Dict.singleton key (updateValue ps new Encode.null)

        [] ->
            Maybe.withDefault Encode.null <| asValue new

        _ ->
            value
