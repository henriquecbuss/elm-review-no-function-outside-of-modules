module NoFunctionOutsideOfModules exposing (rule)

{-|

@docs rule

-}

import Review.Rule as Rule exposing (Rule)


{-| Reports uses of certain functions outside of certain modules.

    config =
        [ NoFunctionOutsideOfModules.rule [ ( "Html.input", [ "View.Input" ] ) ]
        ]


## Fail

Using a qualified name outside of module

    module Main exposing (main)

    import Html

    main : Html.Html a
    main =
        Html.input [] []

Using an exposed name outside of module

    module Main exposing (main)

    import Html exposing (input)

    main : Html.Html a
    main =
        input [] []


## Success

    module View.Input exposing (customInput)

    import Html

    customInput : Html.Html a
    customInput =
        Html.input [] []


## When (not) to enable this rule

This rule is useful when you want people to only use a certain function in
certain modules. This is useful especially for input fields or design elements,
so your app looks consistent overall.
This rule is not useful when you don't think it's worth it to trade consistency
over flexibility.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template NeoVier/elm-review-no-function-outside-of-modules/example --rules NoFunctionOutsideOfModules
```

-}
rule : List ( String, List String ) -> Rule
rule _ =
    Rule.newModuleRuleSchema "NoFunctionOutsideOfModules" ()
        -- Add your visitors
        |> Rule.fromModuleRuleSchema
