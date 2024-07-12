module Json.Schema.Form.Theme exposing (Theme, tailwind)

{-| Theme allows you to set styling of generated Form elements

@docs Theme, tailwind

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Record, that holds the styling of elements
-}
type alias Theme =
    { txt : { withError : Bool } -> Attribute Never
    , categorizationMenu : Attribute Never
    , categorizationMenuItem : { focus : Bool } -> Attribute Never
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
    , horizontalLayout : Attribute Never
    , horizontalLayoutItem : Attribute Never
    , radioWrapper : Attribute Never
    , radioInput : Attribute Never
    , radioInputLabel : Attribute Never
    , fieldLabel : Attribute Never
    , fieldInput : Attribute Never
    , fieldGroup : Attribute Never
    , group : Attribute Never
    , fieldTitle : Attribute Never
    , fieldDescription : Attribute Never
    , liveError : Attribute Never
    , inputGroupPrepend : Attribute Never
    , inputGroupPrependContent : Attribute Never
    , inputGroupAppend : Attribute Never
    , inputGroupAppendContent : Attribute Never
    , inputGroup : Attribute Never
    , label : Attribute Never
    , groupLabel : Attribute Never
    }


tailwind : Theme
tailwind =
    let
        isInvalid =
            "border-1 border-red-500"
    in
    { txt =
        \{ withError } ->
            Attrs.classList
                [ ( "block w-full rounded-md py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6", True )
                , ( "border-0", not withError )
                , ( isInvalid, withError )
                ]
    , categorizationMenu = Attrs.class "flex my-4 bg-indigo-500 font-bold shadow-sm rounded-md ring-1"
    , categorizationMenuItem =
        \{ focus } ->
            Attrs.classList
                [ ( "p-4 max-w-full", True )
                , ( "text-white", focus )
                , ( "text-white/60", not focus )
                ]
    , checkboxWrapper = Attrs.class "flex h-6 items-center"
    , checkboxInput =
        \{ withError } ->
            Attrs.classList
                [ ( "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600", True )
                , ( isInvalid, withError )
                ]
    , checkboxTitle = Attrs.class "ml-3 text-sm leading-6"
    , select =
        \{ withError } ->
            Attrs.classList
                [ ( "block w-full rounded-md py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6", True )
                , ( "border-0", not withError )
                , ( isInvalid, withError )
                ]
    , listGroup = Attrs.class "mb-2"
    , listGroupItem = Attrs.class "border border-gray-300 rounded-md px-4 py-2 mb-2 shadow-sm"
    , listGroupAddItemButton = Attrs.class "rounded-md bg-gray-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-gray-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-gray-600"
    , listGroupRemoveItemButton = Attrs.class "rounded-md bg-white px-2.5 py-1.5 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
    , horizontalLayout = Attrs.class "flex space-x-4"
    , horizontalLayoutItem = Attrs.class "max-w-full flex-grow"
    , radioWrapper = Attrs.class "flex items-center gap-x-3"
    , radioInput = Attrs.class "h-4 w-4 border-gray-300 text-indigo-600 focus:ring-indigo-600"
    , radioInputLabel = Attrs.class "block text-sm font-medium leading-6 text-gray-900"
    , fieldGroup = Attrs.class "my-4"
    , fieldLabel = Attrs.class "block"
    , fieldInput = Attrs.class "field-input mt-0 shadow-sm"
    , fieldTitle = Attrs.class "block text-sm font-medium leading-6 text-gray-900"
    , fieldDescription = Attrs.class "text-sm leading-6 text-gray-600 field-meta"
    , group = Attrs.class "field-input border border-gray-300 rounded-md  p-3  my-3 shadow-sm"
    , liveError = Attrs.class "text-red-500 text-xs my-1"
    , inputGroupPrepend = Attrs.class "inline-flex items-center rounded-l-md border border-r-0 border-gray-300 px-3 text-gray-500 sm:text-sm"
    , inputGroupPrependContent = Attrs.class "text-gray-500 sm:text-sm"
    , inputGroupAppend = Attrs.class "inline-flex items-center rounded-r-md border border-l-0 border-gray-300 px-3 text-gray-500 sm:text-sm"
    , inputGroupAppendContent = Attrs.class "text-gray-500 sm:text-sm"
    , inputGroup = Attrs.class "mt-2 flex shadow-sm"
    , listGroupAddItemButtonDefaultTitle = "Add"
    , listGroupRemoveItemButtonTitle = "Remove"
    , label = Attrs.class "text-lg mt-4"
    , groupLabel = Attrs.class "text-lg"
    }
