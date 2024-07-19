module Settings exposing (errorString, initForm)

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Theme as Theme
import Json.Encode as Encode
import Json.Schema.Definitions exposing (Schema)
import UiSchema exposing (UiSchema)


initForm : String -> Schema -> Maybe UiSchema -> Form
initForm =
    Form.init
        { errors = always errorString
        , theme = Theme.tailwind
        }


errorString : ErrorValue -> String
errorString error =
    case error of
        Empty ->
            "is a required property"

        NotConst v ->
            case Encode.encode 0 v of
                "true" ->
                    "must be checked"

                "false" ->
                    "must be unchecked"

                s ->
                    "must be equal to " ++ s

        InvalidString ->
            "not a valid string"

        InvalidEmail ->
            "not a valid email address"

        InvalidFormat _ ->
            "not the correct format"

        InvalidInt ->
            "not a valid integer"

        InvalidFloat ->
            "not a valid number"

        InvalidBool ->
            "not a valid option"

        InvalidNull ->
            "not a null"

        LessIntThan n ->
            "can not be smaller than " ++ String.fromInt n

        LessEqualIntThan n ->
            "can not be smaller or equal than " ++ String.fromInt n

        GreaterIntThan n ->
            "can not be greater than " ++ String.fromInt n

        GreaterEqualIntThan n ->
            "can not be greater or equal than " ++ String.fromInt n

        LessFloatThan n ->
            "can not be smaller than " ++ String.fromFloat n

        LessEqualFloatThan n ->
            "can not be smaller or equal than " ++ String.fromFloat n

        GreaterFloatThan n ->
            "can not be greater than " ++ String.fromFloat n

        GreaterEqualFloatThan n ->
            "can not be greater or equal than " ++ String.fromFloat n

        ShorterStringThan n ->
            "must NOT have fewer than " ++ String.fromInt n ++ " characters"

        LongerStringThan n ->
            "must NOT have more than " ++ String.fromInt n ++ " characters"

        NotMultipleOfInt n ->
            "must be a multiple of " ++ String.fromInt n ++ "."

        NotMultipleOfFloat n ->
            "must be a multiple of " ++ String.fromFloat n ++ "."

        NotIncludedIn _ ->
            "is not a valid selection from the list."

        Unimplemented s ->
            "unimplemented: " ++ s
