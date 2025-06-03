module UiSchema.Rule exposing (AppliedEffect(..), computeRule)

import Form.Validation exposing (validate)
import Json.Decode exposing (Value)
import Json.Pointer as Pointer
import UiSchema.Internal as UI exposing (Effect(..))
import Validation


type AppliedEffect
    = Hidden
    | Disabled


computeRule : Value -> Maybe UI.Rule -> Maybe AppliedEffect
computeRule formValue mRule =
    let
        condition rule =
            case Pointer.pointedValue rule.condition.scope formValue of
                Nothing ->
                    False

                Just v ->
                    Validation.isOk <| validate rule.condition.schema v

        go rule =
            case ( rule.effect, condition rule ) of
                ( EffectDisable, True ) ->
                    Just Disabled

                ( EffectEnable, False ) ->
                    Just Disabled

                ( EffectShow, False ) ->
                    Just Hidden

                ( EffectHide, True ) ->
                    Just Hidden

                _ ->
                    Nothing
    in
    Maybe.andThen go mRule
