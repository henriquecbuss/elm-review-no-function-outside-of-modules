module NoFunctionOutsideOfModules exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports uses of certain functions outside of certain modules.

    config =
        [ NoFunctionOutsideOfModules.rule [ ( [ "Html.input" ], [ "View.Input" ] ) ]
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

Consider changing your API before enabling this rule. It's usually better to have
a good API that can enforce usage patterns than to rely on a rule.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template henriquecbuss/elm-review-no-function-outside-of-modules/example-with-no-html-input-outside-of-view --rules NoFunctionOutsideOfModules
```

-}
rule : List ( List String, List String ) -> Rule
rule config =
    Rule.newModuleRuleSchemaUsingContextCreator "NoFunctionOutsideOfModules" initialContext
        |> Rule.withExpressionEnterVisitor (expressionVisitor config)
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable moduleName () ->
            { lookupTable = lookupTable
            , moduleName = moduleName
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


expressionVisitor : List ( List String, List String ) -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor config node context =
    case Node.value node of
        Expression.FunctionOrValue _ functionName ->
            case Review.ModuleNameLookupTable.fullModuleNameFor context.lookupTable node of
                Nothing ->
                    ( [], context )

                Just moduleName ->
                    let
                        fullyQualifiedName : String
                        fullyQualifiedName =
                            String.join "." moduleName ++ "." ++ functionName

                        currentModuleName : String
                        currentModuleName =
                            String.join "." context.moduleName

                        modulesToUseInErrorDetails : List String
                        modulesToUseInErrorDetails =
                            List.filterMap
                                (\( forbiddenFunctions, allowedModules ) ->
                                    if
                                        List.member fullyQualifiedName forbiddenFunctions
                                            && not (List.member currentModuleName allowedModules)
                                    then
                                        Just allowedModules

                                    else
                                        Nothing
                                )
                                config
                                |> List.concat
                    in
                    if List.isEmpty modulesToUseInErrorDetails then
                        ( [], context )

                    else
                        ( [ Rule.error
                                { message =
                                    "You're using the `"
                                        ++ fullyQualifiedName
                                        ++ "` function outside of the allowed modules"
                                , details =
                                    [ "The `"
                                        ++ fullyQualifiedName
                                        ++ "` function is only allowed to be used in these modules:\n\n"
                                        ++ (List.map (\allowedModule -> "\t`" ++ allowedModule ++ "`") modulesToUseInErrorDetails
                                                |> String.join "\n"
                                           )
                                    ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

        _ ->
            ( [], context )
