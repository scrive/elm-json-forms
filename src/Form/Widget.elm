module Form.Widget exposing
    ( Widget(..)
    , Label
    , Group
    , Categorization
    , CategoryButton
    , Validation(..)
    , isInvalid
    , Control(..)
    , Options
    , Checkbox
    , TextInput
    , TextArea
    , Slider
    , RadioGroup
    , Select
    )


{-| Abstract form view representation.

@docs Widget, Label, Group, Categorization, CategoryButton, Control, Options, Validation, isInvalid, Checkbox, TextInput, TextArea, Slider, RadioGroup, Select

-}

import Form.Error exposing (ErrorValue)
import Form.FieldValue exposing (FieldType)
import Form.State exposing (Msg)


{-| Root of the form representation.

Widgets can be nested to create more complex forms.
-}
type Widget
    = WHorizontalLayout (List Widget)
    | WVerticalLayout (List Widget)
    | WGroup Group
    | WCategorization Categorization
    | WLabel Label
    | WControl Options Control


{-| Label element.

-}
type alias Label =
    String


{-| Labeled group of elements.

Elements in a group are rendered vertically, below the group label.
-}
type alias Group =
    { label : Maybe String
    , elements : List Widget
    }


{-| Categorization element.

A categorization element consists of a list of category buttons,
and a list of elements from the chosen category.

Categorization element contains only the active category elements.
-}
type alias Categorization =
    { buttons : List CategoryButton
    , elements : List Widget
    }


{-| Category button.

A category button is a button that can be clicked to select the active category.

Active category is marked with the `focus` attribute.
-}
type alias CategoryButton =
    { label : String
    , focus : Bool
    , onClick : Msg
    }


{-| Control options

Controls should be rendered in accordance with these options.

  - `onFocus` - Used only to show field descriptions on focus.
    If this feature is not used, it does not need to be triggered.

-}
type alias Options =
    { label : Maybe String
    , id : String
    , disabled : Bool
    , required : Bool
    , validation : Validation
    , description : Maybe String
    , onFocus : Msg
    , trim : Bool
    }


{-| Specific kind of a control element with associated options.

-}
type Control
    = CCheckbox Checkbox
    | CTextInput TextInput
    | CTextArea TextArea
    | CSlider Slider
    | CRadioGroup RadioGroup
    | CSelect Select



{-| Validation state of a field.

To avoid showing many validation errors for a freshly-displayed form,
the validation state is not shown until the field is focused, or
until the `validateAll` message is sent.
-}
type Validation
    = NotValidated
    | Valid
    | Invalid ErrorValue


{-| Convenience function to check if a validation state is invalid.

-}
isInvalid : Validation -> Bool
isInvalid validation =
    case validation of
        Invalid _ ->
            True

        _ ->
            False



{-| Checkbox control.

-}
type alias Checkbox =
    { value : Bool
    , onCheck : Bool -> Msg
    }


{-| Text input control.

-}
type alias TextInput =
    { value : String
    , fieldType : FieldType
    , restrict : Maybe Int
    , onInput : String -> Msg
    }


{-| Text area control.

-}
type alias TextArea =
    { value : String
    , restrict : Maybe Int
    , onInput : String -> Msg
    }


{-| Slider control.

This element is implemented for completeness, but there are probably not many use-cases for it.
-}
type alias Slider =
    { value : String
    , min : String
    , max : String
    , step : String
    , onInput : String -> Msg
    }


{-| Radio group control.

-}
type alias RadioGroup =
    { value : String
    , valueList : List { id : String, label : String, onClick : Msg }
    , vertical : Bool
    }


{-| Select control.

-}
type alias Select =
    { value : String
    , valueList : List String
    , onChange : String -> Msg
    }
