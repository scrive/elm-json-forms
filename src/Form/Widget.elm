module Form.Widget exposing (Widget(..), Group, Categorization, CategoryButton, Label, Options, Validation(..), isInvalid, Control(..), TextInput, FieldFormat(..), FieldType(..), TextArea, Select, RadioGroup, Checkbox, Slider)

{-| Abstract form view representation.

This representation is meant to be rendered into HTML. For inspiration, see the
[`Form.Widget.View`](https://github.com/scrive/elm-json-forms/blob/main/src/Form/Widget/View.elm) module.

@docs Widget, Group, Categorization, CategoryButton, Label, Options, Validation, isInvalid, Control, TextInput, FieldFormat, FieldType, TextArea, Select, RadioGroup, Checkbox, Slider

-}

import Form.Error exposing (ErrorValue)
import Form.State exposing (Msg)


{-| Root of the form representation.

Widgets can be nested to create complex forms.

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

`elements` list contains the active category elements,
which should be rendered vertically below category buttons.

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

  - `label` - Label of the control, to be shown above the control.
  - `id` - Unique identifier for the control, to be used as the "id" attribute.
  - `required` - Whether the control should be marked as required, with an asterisk or similar.
    Note that the control may or may not _actually_ be required.
  - `validation` - Validation state of the control, possibly containing an error value.
  - `description` - Description of the control, to be shown below the control.
  - `onFocus` - Used only to show field descriptions on focus.
    If this feature is not used, the `onFocus` message does not need to be triggered.
  - `trim` - Whether the control should be short

-}
type alias Options =
    { label : Maybe String
    , ariaLabel : String
    , id : String
    , disabled : Bool
    , required : Bool
    , validation : Validation
    , description : Maybe String
    , onFocus : Msg
    , trim : Bool
    }


{-| Validation state of a field.

To avoid showing many validation errors for a freshly-displayed form,
the validation state is `NotValidated` until the field is focused, or
until the `validateAllFieldsMsg` message is sent.

-}
type Validation
    = NotValidated
    | Valid
    | Invalid ErrorValue


{-| Convenience function to check if a validation state is invalid.

If so, the associated error value should be rendered.

-}
isInvalid : Validation -> Bool
isInvalid validation =
    case validation of
        Invalid _ ->
            True

        _ ->
            False


{-| Specific kind of a control element with associated options.
-}
type Control
    = CTextInput TextInput
    | CTextArea TextArea
    | CSelect Select
    | CRadioGroup RadioGroup
    | CCheckbox Checkbox
    | CSlider Slider


{-| Text input control

`maxLength` should inform the "maxlength" attribute of the input field.

-}
type alias TextInput =
    { value : String
    , fieldType : FieldType
    , maxLength : Maybe Int
    , onInput : String -> Msg
    }


{-| Type of a text-like input field
-}
type FieldType
    = NumberField
    | IntField
    | StringField FieldFormat


{-| Format of a string field
-}
type FieldFormat
    = Text
    | Email
    | Date
    | Time
    | DateTime
    | Phone


{-| Text area control

`maxLength` should inform the "maxlength" attribute of the input field.

-}
type alias TextArea =
    { value : String
    , maxLength : Maybe Int
    , onInput : String -> Msg
    }


{-| Select control.
-}
type alias Select =
    { valueList : List { label : String, selected : Bool }
    , onChange : String -> Msg
    }


{-| Radio group control.
-}
type alias RadioGroup =
    { valueList : List { id : String, label : String, checked : Bool, onClick : Msg }
    , vertical : Bool
    }


{-| Checkbox control
-}
type alias Checkbox =
    { value : Bool
    , onCheck : Bool -> Msg
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
