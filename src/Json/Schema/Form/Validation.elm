module Json.Schema.Form.Validation exposing (validation)

import Dict exposing (Dict)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field
import Form.Validate as Validate exposing (Validation)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Json.Schema.Definitions
    exposing
        ( ExclusiveBoundary(..)
        , Items(..)
        , Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        , blankSchema
        )
import Json.Schema.Form.Format exposing (Format)
import Json.Schema.Form.Regex
import Json.Schema.Form.UiSchema exposing (unSchemata)
import Maybe.Extra as Maybe
import Regex
import Set


type alias Formats =
    Dict String Format


validation : Schema -> Value -> Validation Value
validation schema value =
    case schema of
        BooleanSchema bool ->
            if bool then
                Validate.succeed value

            else
                Validate.fail (Error.error <| Unimplemented "Boolean schemas are not implemented.")

        ObjectSchema objectSchema ->
            subSchema objectSchema value


subSchema : SubSchema -> Value -> Validation Value
subSchema schema =
    let
        typeValidations : Value -> Validation Value
        typeValidations =
            case schema.type_ of
                SingleType type_ ->
                    validateSingleType schema type_

                AnyType -> Validate.succeed

                NullableType type_ ->
                    Validate.oneOf
                        [ \v -> Result.map (always Encode.null) <| validateNull schema v
                        , validateSingleType schema type_
                        ]

                UnionType types ->
                    Validate.oneOf <| List.map (\type_ -> validateSingleType schema type_) types
    in
        Validate.validateAll
            [ Validate.whenJust schema.const validateConst
            , Validate.whenJust schema.enum validateEnum
            , typeValidations
            ]


validateSingleType : SubSchema -> SingleType -> Value -> Validation Value
validateSingleType schema type_ value =
    case type_ of
        ObjectType ->
            let
                propList =
                    Maybe.withDefault [] <| Maybe.map unSchemata schema.properties

                requiredKeys =
                    Set.fromList <| Maybe.withDefault [] schema.required

                mValue key =
                    Result.toMaybe <| Decode.decodeValue (Decode.field key Decode.value) value

                validateKey key propSchema =
                    Validate.mapErrorPointers (List.append [ "properties", key ]) <|
                        case ( mValue key, Set.member key requiredKeys ) of
                            ( Nothing, True ) ->
                                Err [ ( [], Empty ) ]

                            ( Nothing, False ) ->
                                Ok Encode.null

                            ( Just val, _ ) ->
                                validation propSchema val
            in
            Validate.validateAll (List.map (\( key, propSchema ) _ -> validateKey key propSchema) propList) value

        IntegerType ->
            Result.map Encode.int <| validateInt schema value

        NumberType ->
            Result.map Encode.float <| validateFloat schema value

        BooleanType ->
            Result.map Encode.bool <| validateBool schema value

        StringType ->
            Result.map Encode.string <| validateString schema value

        NullType ->
            Result.map (always Encode.null) <| validateNull schema value

        x ->
            Err <| Error.error (Error.Unimplemented "type")


validateString : SubSchema -> Value -> Validation String
validateString schema v =
    case Decode.decodeValue Decode.string v of
        Err _ ->
            Err <| Error.error Error.InvalidString

        Ok s ->
            Validate.validateAll
                [ Validate.whenJust schema.minLength validateMinLength
                , Validate.whenJust schema.maxLength validateMaxLength
                , Validate.whenJust schema.pattern validatePattern -- TODO: check specs if this is correct
                , Validate.whenJust schema.format validateFormat -- TODO: check specs if this is correct
                ]
                s


validateFormat : String -> String -> Validation String
validateFormat format v =
    case format of
        -- TODO: custom error for different formats.
        "date-time" ->
            validateRegex Json.Schema.Form.Regex.dateTime v

        "date" ->
            validateRegex Json.Schema.Form.Regex.date v

        "time" ->
            validateRegex Json.Schema.Form.Regex.time v

        "email" ->
            validateRegex Json.Schema.Form.Regex.email v

        "hostname" ->
            validateRegex Json.Schema.Form.Regex.hostname v

        "ipv4" ->
            validateRegex Json.Schema.Form.Regex.ipv4 v

        "ipv6" ->
            validateRegex Json.Schema.Form.Regex.ipv6 v

        _ ->
            Validate.succeed v


validatePattern : String -> String -> Validation String
validatePattern pat s =
    case Regex.fromString pat of
        Just regex ->
            validateRegex regex s

        Nothing ->
            Err (Error.error Error.InvalidFormat)


validateRegex : Regex.Regex -> String -> Validation String
validateRegex regex s =
    if Regex.contains regex s then
        Ok s

    else
        Err (Error.error Error.InvalidFormat)



-- TODO: create a more specific error


validateMinLength : Int -> String -> Validation String
validateMinLength i s =
    Validate.unless (String.length s >= i) (Error.ShorterStringThan i) s


validateMaxLength : Int -> String -> Validation String
validateMaxLength i s =
    Validate.unless (String.length s <= i) (Error.LongerStringThan i) s


validateInt : SubSchema -> Value -> Validation Int
validateInt schema v =
    case Decode.decodeValue Decode.int v of
        Err _ ->
            Err <| Error.error Error.InvalidInt

        Ok x ->
            Validate.validateAll
                [ Validate.whenJust (minimum schema) boundedInt
                , Validate.whenJust (maximum schema) boundedInt
                , Validate.whenJust (Maybe.map round schema.multipleOf) multipleOfInt
                ]
                x


validateFloat : SubSchema -> Value -> Validation Float
validateFloat schema v =
    case Decode.decodeValue Decode.float v of
        Err _ ->
            Err <| Error.error Error.InvalidFloat

        Ok x ->
            Validate.validateAll
                [ Validate.whenJust (minimum schema) boundedFloat
                , Validate.whenJust (maximum schema) boundedFloat
                , Validate.whenJust schema.multipleOf multipleOfFloat
                ]
                x


validateBool : SubSchema -> Value -> Validation Bool
validateBool schema v =
    case Decode.decodeValue Decode.bool v of
        Err _ ->
            Err <| Error.error Error.InvalidBool

        Ok x ->
            Ok x


validateNull : SubSchema -> Value -> Validation ()
validateNull schema v =
    case Decode.decodeValue (Decode.null ()) v of
        Err _ ->
            Err <| Error.error Error.InvalidNull

        Ok x ->
            Ok x


validateConst : Value -> Value -> Validation Value
validateConst a b =
    Validate.unless (a == b) (Error.NotConst a) b


validateEnum : List Value -> Value -> Validation Value
validateEnum l a =
    Validate.unless (List.member a l) (Error.NotIncludedIn l) a


multipleOfInt : Int -> Int -> Validation Int
multipleOfInt p a =
    Validate.unless (modBy p a == 0) (Error.NotMultipleOfInt p) a


multipleOfFloat : Float -> Float -> Validation Float
multipleOfFloat p a =
    let
        isMultiple =
            -- rough check that sort-of works with common values
            abs (toFloat (round (a / p)) - (a / p)) < 1.0e-10
    in
    Validate.unless isMultiple (Error.NotMultipleOfFloat p) a


boundedInt : Comparison -> Int -> Validation Int
boundedInt cmp intA =
    Result.map (always intA) <| boundedFloat cmp (toFloat intA)


boundedFloat : Comparison -> Float -> Validation Float
boundedFloat cmp a =
    a
        |> (case cmp of
                LT x ->
                    Validate.unless (a < x) <| Error.GreaterEqualFloatThan x

                LE x ->
                    Validate.unless (a <= x) <| Error.GreaterFloatThan x

                GT x ->
                    Validate.unless (a > x) <| Error.LessEqualFloatThan x

                GE x ->
                    Validate.unless (a >= x) <| Error.LessFloatThan x
           )


type Comparison
    = LT Float
    | LE Float
    | GT Float
    | GE Float


minimum : SubSchema -> Maybe Comparison
minimum schema =
    case schema.exclusiveMinimum of
        Just (BoolBoundary True) ->
            Maybe.map GT schema.minimum

        Just (BoolBoundary False) ->
            Maybe.map GE schema.minimum

        Just (NumberBoundary value) ->
            Just (GT value)

        Nothing ->
            Maybe.map GE schema.minimum


maximum : SubSchema -> Maybe Comparison
maximum schema =
    case schema.exclusiveMaximum of
        Just (BoolBoundary True) ->
            Maybe.map LT schema.maximum

        Just (BoolBoundary False) ->
            Maybe.map LE schema.maximum

        Just (NumberBoundary value) ->
            Just (LT value)

        Nothing ->
            Maybe.map LE schema.maximum
