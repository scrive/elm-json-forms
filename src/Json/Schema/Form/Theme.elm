module Json.Schema.Form.Theme exposing (Theme, tailwind)

{-| Theme allows you to set styling of generated Form elements

@docs Theme, tailwind

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Record, that holds the styling of elements
-}
type alias Theme =
    { categorizationMenu : Attribute Never
    , categorizationMenuItem : { focus : Bool } -> Attribute Never
    , label : Attribute Never
    , groupLabel : Attribute Never

    , fieldGroup : Attribute Never
    , fieldLabel : Attribute Never
    , fieldInput : { withError : Bool } -> Attribute Never
    , fieldDescription : Attribute Never
    , fieldError : Attribute Never

    , checkboxRow : Attribute Never
    , checkboxInput : { withError : Bool } -> Attribute Never

    , horizontalLayout : Attribute Never
    , horizontalLayoutItem : Attribute Never
    , group : Attribute Never
    }


tailwind : Theme
tailwind =
    let
        isInvalid =
            "border-1 border-red-500"
    in
    { categorizationMenu = Attrs.class "flex my-4 bg-indigo-500 font-bold shadow-sm rounded-md ring-1"
    , categorizationMenuItem =
        \{ focus } ->
            Attrs.classList
                [ ( "p-4 max-w-full", True )
                , ( "text-white", focus )
                , ( "text-white/60", not focus )
                ]
    , label = Attrs.class "text-lg mt-4"
    , groupLabel = Attrs.class "text-lg"

    , fieldGroup = Attrs.class "my-4"
    , fieldInput =
        \{ withError } ->
            Attrs.classList
                [ ( "block w-full rounded-md py-1.5 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6", True )
                , ( "border-0", not withError )
                , ( isInvalid, withError )
                ]
    , fieldLabel = Attrs.class "block text-sm leading-6 text-gray-900"
    , fieldDescription = Attrs.class "text-sm leading-6 text-gray-600 field-meta"
    , fieldError = Attrs.class "text-red-500 text-xs my-1"

    , checkboxRow = Attrs.class "flex items-center"
    , checkboxInput =
        \{ withError } ->
            Attrs.classList
                [ ( "h-4 w-4 mr-3 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600", True )
                , ( isInvalid, withError )
                ]

    , horizontalLayout = Attrs.class "flex space-x-4"
    , horizontalLayoutItem = Attrs.class "max-w-full flex-grow"
    , group = Attrs.class "field-input border border-gray-300 rounded-md  p-3  my-3 shadow-sm"
    }
