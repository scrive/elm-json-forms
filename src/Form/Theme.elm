module Form.Theme exposing (Theme, tailwind)

{-| Form appearance

@docs Theme, tailwind

-}

import Html exposing (Attribute)
import Html.Attributes as Attrs


{-| Form appearance definition
-}
type alias Theme =
    { horizontalLayout : Attribute Never
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
    , textInput : { trim : Bool, invalid : Bool } -> Attribute Never
    , textArea : { trim : Bool, invalid : Bool } -> Attribute Never
    , selectInput : { trim : Bool, invalid : Bool } -> Attribute Never
    , checkboxInput : { invalid : Bool } -> Attribute Never
    , sliderInput : { trim : Bool } -> Attribute Never
    , group : Attribute Never
    , disabledElems : Attribute Never
    }


{-| Default form styling using TailWind

You can modify this according to your needs.

-}
tailwind : Theme
tailwind =
    { horizontalLayout = Attrs.class "flex space-x-4"
    , horizontalLayoutItem = Attrs.class "flex-grow"
    , label = Attrs.class "text-lg mt-4"
    , groupLabel = Attrs.class "text-lg"
    , categorizationMenu = Attrs.class "my-4 bg-indigo-500 font-bold"
    , categorizationMenuItem =
        \{ focus } ->
            Attrs.classList
                [ ( "p-4", True )
                , ( "text-white", focus )
                , ( "text-white/60", not focus )
                ]
    , fieldGroup = Attrs.class "my-4"
    , fieldLabel = Attrs.class "block text-sm my-1"
    , fieldDescription = Attrs.class "text-sm text-gray-600"
    , fieldError = Attrs.class "text-red-600 text-xs my-1"
    , checkboxRow = Attrs.class "flex items-center"
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
                [ ( "mr-3", True )
                , ( "border-red-600", invalid )
                ]
    , sliderInput =
        \{ trim } ->
        Attrs.classList
                [ ( "w-full", not trim )
                ]
    , group = Attrs.class "border border-gray-300 p-3 my-3"
    , disabledElems = Attrs.class "opacity-50"
    }
