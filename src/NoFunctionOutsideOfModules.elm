module NoFunctionOutsideOfModules exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
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
    Rule.newModuleRuleSchema "NoFunctionOutsideOfModules" AllowedModule
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type Context
    = AllowedModule
    | DisallowedModule ModuleImportStatus


type ModuleImportStatus
    = ModuleWasImported (List String) FunctionImportStatus
    | ModuleWasNotImported


type FunctionImportStatus
    = FunctionWasImportedExplicitly
    | FunctionWasNotImportedExplicitly


moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor node _ =
    if (Node.value node |> Module.moduleName) == [ "View", "Input" ] then
        ( [], AllowedModule )

    else
        ( [], DisallowedModule ModuleWasNotImported )


importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node context =
    case context of
        AllowedModule ->
            ( [], context )

        DisallowedModule _ ->
            case Node.value node |> .moduleName |> Node.value of
                [ "Html" ] ->
                    let
                        moduleAlias =
                            Node.value node
                                |> .moduleAlias
                                |> Maybe.map Node.value
                                |> Maybe.withDefault [ "Html" ]

                        functionImportStatus =
                            case Node.value node |> .exposingList |> Maybe.map Node.value of
                                Just (Exposing.All _) ->
                                    FunctionWasImportedExplicitly

                                Just (Exposing.Explicit exposedFunctions) ->
                                    let
                                        isForbiddenFunction : Node Exposing.TopLevelExpose -> Bool
                                        isForbiddenFunction exposeNode =
                                            case Node.value exposeNode of
                                                Exposing.FunctionExpose "input" ->
                                                    True

                                                _ ->
                                                    False
                                    in
                                    if List.any isForbiddenFunction exposedFunctions then
                                        FunctionWasImportedExplicitly

                                    else
                                        FunctionWasNotImportedExplicitly

                                _ ->
                                    FunctionWasNotImportedExplicitly
                    in
                    ( [], DisallowedModule (ModuleWasImported moduleAlias functionImportStatus) )

                _ ->
                    ( [], DisallowedModule ModuleWasNotImported )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case context of
        AllowedModule ->
            ( [], context )

        DisallowedModule ModuleWasNotImported ->
            ( [], context )

        DisallowedModule (ModuleWasImported moduleAlias functionImportStatus) ->
            case Node.value node of
                Expression.FunctionOrValue moduleName "input" ->
                    let
                        isFromModule =
                            case functionImportStatus of
                                FunctionWasImportedExplicitly ->
                                    True

                                FunctionWasNotImportedExplicitly ->
                                    moduleName == moduleAlias
                    in
                    if isFromModule then
                        ( [ Rule.error
                                { message = "You're using the `Html.input` function outside of the allowed modules"
                                , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\t`View.Input`" ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    else
                        ( [], context )

                _ ->
                    ( [], context )
