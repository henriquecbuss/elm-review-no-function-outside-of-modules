module NoFunctionOutsideOfModules exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import List.Extra
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
rule : ( String, List String ) -> Rule
rule ( forbiddenFunction, allowedModules ) =
    let
        ( forbiddenFunctionName, forbiddenFunctionModule ) =
            forbiddenFunction
                |> String.split "."
                |> List.Extra.unconsLast
                |> Maybe.withDefault ( "", [] )
    in
    Rule.newModuleRuleSchema "NoFunctionOutsideOfModules" AllowedModule
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor (List.map (String.split ".") allowedModules))
        |> Rule.withImportVisitor (importVisitor forbiddenFunctionModule forbiddenFunctionName)
        |> Rule.withExpressionEnterVisitor (expressionVisitor forbiddenFunctionName allowedModules)
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


moduleDefinitionVisitor : List (List String) -> Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor allowedModules node _ =
    if List.member (Node.value node |> Module.moduleName) allowedModules then
        ( [], AllowedModule )

    else
        ( [], DisallowedModule ModuleWasNotImported )


importVisitor : List String -> String -> Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor forbiddenFunctionModule forbiddenFunctionName node context =
    case context of
        AllowedModule ->
            ( [], context )

        DisallowedModule _ ->
            if (Node.value node |> .moduleName |> Node.value) == forbiddenFunctionModule then
                let
                    moduleAlias =
                        Node.value node
                            |> .moduleAlias
                            |> Maybe.map Node.value
                            |> Maybe.withDefault forbiddenFunctionModule

                    functionImportStatus =
                        case Node.value node |> .exposingList |> Maybe.map Node.value of
                            Just (Exposing.All _) ->
                                FunctionWasImportedExplicitly

                            Just (Exposing.Explicit exposedFunctions) ->
                                let
                                    isForbiddenFunction : Node Exposing.TopLevelExpose -> Bool
                                    isForbiddenFunction exposeNode =
                                        case Node.value exposeNode of
                                            Exposing.FunctionExpose exposedFunction ->
                                                exposedFunction == forbiddenFunctionName

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

            else
                ( [], DisallowedModule ModuleWasNotImported )


expressionVisitor : String -> List String -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor forbiddenFunctionName allowedModules node context =
    case context of
        AllowedModule ->
            ( [], context )

        DisallowedModule ModuleWasNotImported ->
            ( [], context )

        DisallowedModule (ModuleWasImported moduleAlias functionImportStatus) ->
            case Node.value node of
                Expression.FunctionOrValue moduleName functionName ->
                    let
                        isFromModule =
                            case functionImportStatus of
                                FunctionWasImportedExplicitly ->
                                    True

                                FunctionWasNotImportedExplicitly ->
                                    moduleName == moduleAlias

                        isForbiddenFunction =
                            functionName == forbiddenFunctionName
                    in
                    if isFromModule && isForbiddenFunction then
                        let
                            fullFunctionName =
                                case functionImportStatus of
                                    FunctionWasImportedExplicitly ->
                                        functionName

                                    FunctionWasNotImportedExplicitly ->
                                        moduleName ++ [ functionName ] |> String.join "."
                        in
                        ( [ Rule.error
                                { message = "You're using the `" ++ fullFunctionName ++ "` function outside of the allowed modules"
                                , details =
                                    [ "The `"
                                        ++ fullFunctionName
                                        ++ "` function is only allowed to be used in these modules:\n\n"
                                        ++ (List.map (\allowedModule -> "\t`" ++ allowedModule ++ "`") allowedModules
                                                |> String.join "\n"
                                           )
                                    ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    else
                        ( [], context )

                _ ->
                    ( [], context )
