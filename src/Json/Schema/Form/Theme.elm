module Json.Schema.Form.Theme exposing (Theme, default, tailwind)

{-| Theme allows you to set styling of generated Form elements

@docs Theme, default, tailwind

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Record, that holds the styling of elements
-}
type alias Theme =
    { txt : { withError : Bool } -> Attribute Never
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
    , fieldInputDescription : Attribute Never
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
        \{ withError } ->
            Attrs.classList
                [ ( "form-control", True )
                , ( "is-invalid", withError )
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
    , fieldInputDescription = Attrs.class "field-meta"
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


{-| Optional tailwindcss theme
-}
tailwind : Theme
tailwind =
    let
        isInvalid =
            "border-2 border-red-500"
    in
    { default
        | -- inputs
          txt =
            \{ withError } ->
                Attrs.classList
                    [ ( "block w-full rounded-md py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6", True )
                    , ( "border-0", not withError )
                    , ( isInvalid, withError )
                    ]

        -- checkbox
        , checkboxWrapper = Attrs.class "flex h-6 items-center"
        , checkboxInput =
            \{ withError } ->
                Attrs.classList
                    [ ( "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600", True )
                    , ( isInvalid, withError )
                    ]
        , checkboxTitle = Attrs.class "ml-3 text-sm leading-6"

        -- select
        , select =
            \{ withError } ->
                Attrs.classList
                    [ ( "block w-full mt-2 rounded-md py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6", True )
                    , ( "border-0", not withError )
                    , ( isInvalid, withError )
                    ]

        -- list group
        , listGroup = Attrs.class "mb-2"
        , listGroupItem = Attrs.class "border border-gray-300 rounded-md px-4 py-2 mb-2 shadow-sm"
        , listGroupAddItemButton = Attrs.class "rounded-md bg-gray-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-gray-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-gray-600"
        , listGroupRemoveItemButton = Attrs.class "rounded-md bg-white px-2.5 py-1.5 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50"

        -- tuple
        , formRow = Attrs.class "flex space-x-4"
        , formRowItem = Attrs.class "max-w-full flex-grow"

        -- radio
        , radioWrapper = Attrs.class "flex items-center gap-x-3"
        , radioInput = Attrs.class "h-4 w-4 border-gray-300 text-indigo-600 focus:ring-indigo-600"
        , radioInputLabel = Attrs.class "block text-sm font-medium leading-6 text-gray-900"
        , field =
            \{ withError, withValue } ->
                Attrs.classList
                    [ ( "mb-6", True )
                    , ( "text-red-500", withError )
                    , ( "has-value", withValue )
                    ]
        , fieldLabel = Attrs.class "block"
        , fieldInput = Attrs.class "field-input"
        , fieldInputDescription = Attrs.class "field-meta"
        , fieldTitle = Attrs.class "block text-sm font-medium leading-6 text-gray-900"
        , fieldDescription = Attrs.class "mt-2 text-sm leading-6 text-gray-600"
        , group =
            \{ withError, withValue } ->
                Attrs.classList
                    [ ( "mb-4", True )
                    , ( "text-red-500", withError )
                    , ( "has-value", withValue )
                    ]
        , liveError = Attrs.class "text-red-500 text-xs my-2"
        , inputGroupPrepend = Attrs.class "inline-flex items-center rounded-l-md border border-r-0 border-gray-300 px-3 text-gray-500 sm:text-sm"
        , inputGroupPrependContent = Attrs.class "text-gray-500 sm:text-sm"
        , inputGroupAppend = Attrs.class "inline-flex items-center rounded-r-md border border-l-0 border-gray-300 px-3 text-gray-500 sm:text-sm"
        , inputGroupAppendContent = Attrs.class "text-gray-500 sm:text-sm"
        , inputGroup = Attrs.class "mt-2 flex shadow-sm"
    }
