module Form.Validation exposing (validation)

import Form.Error as Error exposing (ErrorValue(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Json.Schema.Definitions
    exposing
        ( ExclusiveBoundary(..)
        , Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        )
import Form.Regex
import UiSchema exposing (unSchemata)
import Regex
import Set
import Validation exposing (Validation)


validation : Schema -> Value -> Validation Value
validation schema value =
    case schema of
        BooleanSchema bool ->
            if bool then
                Validation.succeed value

            else
                Validation.fail (Error.error <| Unimplemented "Boolean schemas are not implemented.")

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

                AnyType ->
                    Validation.succeed

                NullableType type_ ->
                    Validation.oneOf
                        [ \v -> Result.map (always Encode.null) <| validateNull v
                        , validateSingleType schema type_
                        ]

                UnionType types ->
                    Validation.oneOf <| List.map (\type_ -> validateSingleType schema type_) types
    in
    Validation.validateAll
        [ Validation.whenJust schema.const validateConst
        , Validation.whenJust schema.enum validateEnum
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
                    Validation.mapErrorPointers (List.append [ "properties", key ]) <|
                        case ( mValue key, Set.member key requiredKeys ) of
                            ( Nothing, True ) ->
                                Err [ ( [], Empty ) ]

                            ( Nothing, False ) ->
                                Ok Encode.null

                            ( Just val, _ ) ->
                                validation propSchema val
            in
            Validation.validateAll (List.map (\( key, propSchema ) _ -> validateKey key propSchema) propList) value

        IntegerType ->
            Result.map Encode.int <| validateInt schema value

        NumberType ->
            Result.map Encode.float <| validateFloat schema value

        BooleanType ->
            Result.map Encode.bool <| validateBool value

        StringType ->
            Result.map Encode.string <| validateString schema value

        NullType ->
            Result.map (always Encode.null) <| validateNull value

        _ ->
            Err <| Error.error (Error.Unimplemented "type")


validateString : SubSchema -> Value -> Validation String
validateString schema v =
    case Decode.decodeValue Decode.string v of
        Err _ ->
            Err <| Error.error Error.InvalidString

        Ok s ->
            Validation.validateAll
                [ Validation.whenJust schema.minLength validateMinLength
                , Validation.whenJust schema.maxLength validateMaxLength
                , Validation.whenJust schema.pattern validatePattern -- TODO: check specs if this is correct
                , Validation.whenJust schema.format validateFormat -- TODO: check specs if this is correct
                ]
                s


validateFormat : String -> String -> Validation String
validateFormat format v =
    case format of
        -- TODO: custom error for different formats.
        "date-time" ->
            validateRegex Form.Regex.dateTime v

        "date" ->
            validateRegex Form.Regex.date v

        "time" ->
            validateRegex Form.Regex.time v

        "email" ->
            validateRegex Form.Regex.email v

        "hostname" ->
            validateRegex Form.Regex.hostname v

        "ipv4" ->
            validateRegex Form.Regex.ipv4 v

        "ipv6" ->
            validateRegex Form.Regex.ipv6 v

        _ ->
            Validation.succeed v


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
    Validation.unless (String.length s >= i) (Error.ShorterStringThan i) s


validateMaxLength : Int -> String -> Validation String
validateMaxLength i s =
    Validation.unless (String.length s <= i) (Error.LongerStringThan i) s


validateInt : SubSchema -> Value -> Validation Int
validateInt schema v =
    case Decode.decodeValue Decode.int v of
        Err _ ->
            Err <| Error.error Error.InvalidInt

        Ok x ->
            Validation.validateAll
                [ Validation.whenJust (minimum schema) boundedInt
                , Validation.whenJust (maximum schema) boundedInt
                , Validation.whenJust (Maybe.map round schema.multipleOf) multipleOfInt
                ]
                x


validateFloat : SubSchema -> Value -> Validation Float
validateFloat schema v =
    case Decode.decodeValue Decode.float v of
        Err _ ->
            Err <| Error.error Error.InvalidFloat

        Ok x ->
            Validation.validateAll
                [ Validation.whenJust (minimum schema) boundedFloat
                , Validation.whenJust (maximum schema) boundedFloat
                , Validation.whenJust schema.multipleOf multipleOfFloat
                ]
                x


validateBool : Value -> Validation Bool
validateBool v =
    case Decode.decodeValue Decode.bool v of
        Err _ ->
            Err <| Error.error Error.InvalidBool

        Ok x ->
            Ok x


validateNull : Value -> Validation ()
validateNull v =
    case Decode.decodeValue (Decode.null ()) v of
        Err _ ->
            Err <| Error.error Error.InvalidNull

        Ok x ->
            Ok x


validateConst : Value -> Value -> Validation Value
validateConst a b =
    Validation.unless (a == b) (Error.NotConst a) b


validateEnum : List Value -> Value -> Validation Value
validateEnum l a =
    Validation.unless (List.member a l) (Error.NotIncludedIn l) a


multipleOfInt : Int -> Int -> Validation Int
multipleOfInt p a =
    Validation.unless (modBy p a == 0) (Error.NotMultipleOfInt p) a


multipleOfFloat : Float -> Float -> Validation Float
multipleOfFloat p a =
    let
        isMultiple =
            -- rough check that sort-of works with common values
            abs (toFloat (round (a / p)) - (a / p)) < 1.0e-10
    in
    Validation.unless isMultiple (Error.NotMultipleOfFloat p) a


boundedInt : Comparison -> Int -> Validation Int
boundedInt cmp intA =
    Result.map (always intA) <| boundedFloat cmp (toFloat intA)


boundedFloat : Comparison -> Float -> Validation Float
boundedFloat cmp a =
    a
        |> (case cmp of
                LT x ->
                    Validation.unless (a < x) <| Error.GreaterEqualFloatThan x

                LE x ->
                    Validation.unless (a <= x) <| Error.GreaterFloatThan x

                GT x ->
                    Validation.unless (a > x) <| Error.LessEqualFloatThan x

                GE x ->
                    Validation.unless (a >= x) <| Error.LessFloatThan x
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
