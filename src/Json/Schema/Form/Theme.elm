module Json.Schema.Form.Theme exposing (Theme, default)

{-| Theme allows you to set styling of generated Form elements

@docs Theme, default

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Record, that holds the styling of elements
-}
type alias Theme =
    { txt : { withError : Bool, format : Maybe String } -> Attribute Never
    , checkboxWrapper : Attribute Never
    , checkboxInput : { withError : Bool } -> Attribute Never
    , checkboxTitle : Attribute Never
    , select : { withError : Bool } -> Attribute Never
    , listGroup : Attribute Never
    , listGroupItem : Attribute Never
    , listGroupAddItemButton : Attribute Never
    , listGroupAddItemButtonDefaultTitle : String
    , listGroupRemoveItemButton : Attribute Never
    , listGroupRemoveItemButtonTitle : String
    , formRow : Attribute Never
    , formRowItem : Attribute Never
    , radioWrapper : Attribute Never
    , radioInput : Attribute Never
    , radioInputLabel : Attribute Never
    , field : { withError : Bool, withValue : Bool } -> Attribute Never
    , fieldLabel : Attribute Never
    , fieldInput : Attribute Never
    , fieldInputMeta : Attribute Never
    , group : { withError : Bool, withValue : Bool } -> Attribute Never
    , fieldTitle : Attribute Never
    , fieldDescription : Attribute Never
    , liveError : Attribute Never
    , inputGroupPrepend : Attribute Never
    , inputGroupPrependContent : Attribute Never
    , inputGroupAppend : Attribute Never
    , inputGroupAppendContent : Attribute Never
    , inputGroup : Attribute Never
    }


{-| Default bootstrap theme
-}
default : Theme
default =
    { -- inputs
      txt =
        \{ withError, format } ->
            Attrs.classList
                [ ( "form-control", True )
                , ( "is-invalid", withError )
                , case format of
                    Just str ->
                        ( "format-" ++ str, True )

                    Nothing ->
                        ( "", False )
                ]

    -- checkbox
    , checkboxWrapper = Attrs.class "checkbox"
    , checkboxInput =
        \{ withError } ->
            Attrs.classList
                [ ( "form-check-input", True )
                , ( "is-invalid", withError )
                ]
    , checkboxTitle = Attrs.class ""

    -- select
    , select =
        \{ withError } ->
            Attrs.classList
                [ ( "form-control custom-select", True )
                , ( "is-invalid", withError )
                ]

    -- list group
    , listGroup = Attrs.class "list-group mb-2"
    , listGroupItem = Attrs.class "list-group-item"
    , listGroupAddItemButton = Attrs.class "btn btn-secondary btn-add"
    , listGroupAddItemButtonDefaultTitle = "Add"
    , listGroupRemoveItemButton = Attrs.class "btn btn-outline-secondary btn-sm btn-remove"
    , listGroupRemoveItemButtonTitle = "Remove"

    -- tuple
    , formRow = Attrs.class "form-row"
    , formRowItem = Attrs.class "col"

    -- radio
    , radioWrapper = Attrs.class ""
    , radioInput = Attrs.class "form-check-input"
    , radioInputLabel = Attrs.class "label-text"
    , field =
        \{ withError, withValue } ->
            Attrs.classList
                [ ( "form-group", True )
                , ( "is-invalid", withError )
                , ( "has-value", withValue )
                ]
    , fieldLabel = Attrs.class "d-block"
    , fieldInput = Attrs.class "field-input"
    , fieldInputMeta = Attrs.class "field-meta"
    , fieldTitle = Attrs.class "label-text"
    , fieldDescription = Attrs.class "form-text text-muted"
    , group =
        \{ withError, withValue } ->
            Attrs.classList
                [ ( "form-group", True )
                , ( "is-invalid", withError )
                , ( "has-value", withValue )
                ]
    , liveError = Attrs.class "invalid-feedback"
    , inputGroupPrepend = Attrs.class "input-group-prepend"
    , inputGroupPrependContent = Attrs.class "input-group-text"
    , inputGroupAppend = Attrs.class "input-group-append"
    , inputGroupAppendContent = Attrs.class "input-group-text"
    , inputGroup = Attrs.class "input-group"
    }
