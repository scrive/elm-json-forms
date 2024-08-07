module Form.Theme exposing (Theme, simpleTailwind, scrive)

{-| Form appearance

@docs Theme, simpleTailwind, scrive

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Form appearance definition
-}
type alias Theme =
    { horizontalLayout : { cols : Int } -> Attribute Never
    , horizontalLayoutItem : Attribute Never
    , groupLabel : Attribute Never
    , label : Attribute Never
    , categorizationMenu : Attribute Never
    , categorizationMenuItem : { focus : Bool } -> Attribute Never
    , fieldGroup : Attribute Never
    , fieldLabel : Attribute Never
    , fieldDescription : Attribute Never
    , fieldError : Attribute Never
    , checkboxRow : Attribute Never
    , radioEntry : { vertical : Bool } -> Attribute Never
    , textInput : { trim : Bool, invalid : Bool } -> Attribute Never
    , textArea : { trim : Bool, invalid : Bool } -> Attribute Never
    , selectInput : { trim : Bool, invalid : Bool } -> Attribute Never
    , checkboxInput : { invalid : Bool } -> Attribute Never
    , radioInput : Attribute Never
    , sliderInput : Attribute Never
    , sliderWithTicks : { trim : Bool } -> Attribute Never
    , toggleInput : { checked : Bool } -> Attribute Never
    , toggleKnob : { checked : Bool } -> Attribute Never
    , group : Attribute Never
    , disabledElems : Attribute Never
    }


{-| Simple form styling using Tailwind

This theme is as simple as possible while also having acceptable styling.

-}
simpleTailwind : Theme
simpleTailwind =
    { horizontalLayout = \{ cols } -> Attrs.class ("grid gap-3 grid-cols-" ++ String.fromInt cols)
    , horizontalLayoutItem = Attrs.class ""
    , label = Attrs.class "font-bold mt-4"
    , groupLabel = Attrs.class "font-bold"
    , categorizationMenu = Attrs.class "my-4 border-b"
    , categorizationMenuItem =
        \{ focus } ->
            Attrs.classList
                [ ( "p-4 pb-2", True )
                , ( "text-blue-500 border-b-2 border-blue-500", focus )
                , ( "", not focus )
                ]
    , fieldGroup = Attrs.class "my-4"
    , fieldLabel = Attrs.class "block text-sm my-1"
    , fieldDescription = Attrs.class "text-sm text-slate-500 my-1"
    , fieldError = Attrs.class "text-red-600 text-xs my-1"
    , checkboxRow = Attrs.class "flex items-center space-x-4"
    , radioEntry =
        \{ vertical } ->
            Attrs.classList
                [ ( "mr-5 items-center", True )
                , ( "flex", vertical )
                ]
    , textInput =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "w-full", not trim )
                ]
    , textArea =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "w-full", not trim )
                ]
    , selectInput =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "w-full", not trim )
                ]
    , checkboxInput =
        \{ invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                ]
    , radioInput =
        Attrs.classList
            [ ( "mr-3", True )
            ]
    , sliderInput =
        Attrs.class "w-full"
    , sliderWithTicks =
        \{ trim } ->
            Attrs.classList
                [ ( "w-52", trim )
                ]
    , toggleInput =
        \{ checked } ->
            Attrs.classList
                [ ( "inline-flex w-11 rounded-full border-2 border-transparent transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2", True )
                , ( "bg-gray-300", not checked )
                , ( "bg-blue-500", checked )
                ]
    , toggleKnob =
        \{ checked } ->
            Attrs.classList
                [ ( "pointer-events-none h-5 w-5 rounded-full bg-white shadow transition duration-200 ease-in-out", True )
                , ( "translate-x-0", not checked )
                , ( "translate-x-5", checked )
                ]
    , group = Attrs.class "border border-gray-300 p-3 my-3"
    , disabledElems = Attrs.class "opacity-50"
    }


{-| Simple form styling using Tailwind

This theme is as simple as possible while also having acceptable styling.

-}
scrive : Theme
scrive =
    { horizontalLayout = \{ cols } -> Attrs.class ("grid gap-3 grid-cols-" ++ String.fromInt cols)
    , horizontalLayoutItem = Attrs.class ""
    , label = Attrs.class "font-bold mt-4"
    , groupLabel = Attrs.class "font-bold"
    , categorizationMenu = Attrs.class "my-4 border-b"
    , categorizationMenuItem =
        \{ focus } ->
            Attrs.classList
                [ ( "p-4 pb-2", True )
                , ( "text-blue-500 border-b-2 border-blue-500", focus )
                , ( "", not focus )
                ]
    , fieldGroup = Attrs.class "my-4"
    , fieldLabel = Attrs.class "block text-sm font-medium mb-1"
    , fieldDescription = Attrs.class "text-sm text-slate-500 my-1"
    , fieldError = Attrs.class "text-red-600 text-xs my-1"
    , checkboxRow = Attrs.class "flex gap-3"
    , radioEntry =
        \{ vertical } ->
            Attrs.classList
                [ ( "mr-5", True )
                , ( "flex", vertical )
                ]
    , textInput =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "border-gray-700", not invalid )
                , ( "w-full", not trim )
                , ( "w-52", trim )
                , ( "border py-2 px-3 text-sm rounded", True )
                ]
    , textArea =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "border-gray-700", not invalid )
                , ( "w-full", not trim )
                , ( "w-52", trim )
                , ( "border py-2 px-3 text-sm rounded", True )
                ]
    , selectInput =
        \{ trim, invalid } ->
            Attrs.classList
                [ ( "border-red-600", invalid )
                , ( "border-gray-700", not invalid )
                , ( "w-full", not trim )
                , ( "w-52", trim )
                , ( "border py-2 px-3 text-sm rounded bg-white", True )
                ]
    , checkboxInput =
        \_ ->
            Attrs.classList
                [ ( "border-gray-500", True ) -- "border-scrive-gray" eqivalent
                , ( "border-2 w-5 h-5", True )
                ]
    , radioInput =
        Attrs.class "border-gray-500 border-2 w-4 h-4 mr-3"
    , sliderInput =
        Attrs.class "w-full"
    , sliderWithTicks =
        \{ trim } ->
            Attrs.classList
                [ ( "w-52", trim )
                ]
    , toggleInput =
        \{ checked } ->
            Attrs.classList
                [ ( "inline-flex w-11 rounded-full border-2 border-transparent transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2", True )
                , ( "bg-gray-300", not checked )
                , ( "bg-blue-500", checked )
                ]
    , toggleKnob =
        \{ checked } ->
            Attrs.classList
                [ ( "pointer-events-none h-5 w-5 rounded-full bg-white shadow transition duration-200 ease-in-out", True )
                , ( "translate-x-0", not checked )
                , ( "translate-x-5", checked )
                ]
    , group = Attrs.class "border border-gray-300 p-3 my-3"
    , disabledElems = Attrs.class "opacity-50"
    }
