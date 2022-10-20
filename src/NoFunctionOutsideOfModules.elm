module NoFunctionOutsideOfModules exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)
import Set


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
    Rule.newModuleRuleSchema "NoFunctionOutsideOfModules" WithNoForbiddenFunctions
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor config)
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionEnterVisitor (expressionVisitor config)
        |> Rule.fromModuleRuleSchema


type Context
    = WithNoForbiddenFunctions
    | WithForbiddenFunctions (List ForbiddenFunction)


type alias ForbiddenFunction =
    { importStatus : ImportStatus
    , fullName : String
    , qualifiedName : String
    }


type ImportStatus
    = FunctionWasImported
    | FunctionWasImportedExplicitly
    | FunctionWasNotImported


moduleDefinitionVisitor : List ( List String, List String ) -> Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor config node _ =
    let
        moduleName : String
        moduleName =
            Node.value node |> Module.moduleName |> String.join "."

        forbiddenFunctionsInThisModule : List String
        forbiddenFunctionsInThisModule =
            List.foldr
                (\( forbiddenFunctions, allowedModules ) currentForbiddenFunctions ->
                    if List.member moduleName allowedModules then
                        currentForbiddenFunctions

                    else
                        forbiddenFunctions ++ currentForbiddenFunctions
                )
                []
                config
    in
    ( []
    , if List.isEmpty forbiddenFunctionsInThisModule then
        WithNoForbiddenFunctions

      else
        forbiddenFunctionsInThisModule
            |> List.map
                (\forbiddenFunction ->
                    { importStatus = FunctionWasNotImported
                    , fullName = forbiddenFunction
                    , qualifiedName = forbiddenFunction
                    }
                )
            |> WithForbiddenFunctions
    )


getFunctionModuleAndName : String -> ( List String, String )
getFunctionModuleAndName function =
    case String.split "." function |> List.reverse of
        [] ->
            ( [], "" )

        first :: rest ->
            ( List.reverse rest, first )


importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node context =
    case context of
        WithNoForbiddenFunctions ->
            ( [], context )

        WithForbiddenFunctions forbiddenFunctionsStatus ->
            let
                makeImportStatus : ForbiddenFunction -> ForbiddenFunction
                makeImportStatus ({ importStatus, fullName } as existingImportStatus) =
                    case importStatus of
                        FunctionWasImported ->
                            existingImportStatus

                        FunctionWasImportedExplicitly ->
                            existingImportStatus

                        FunctionWasNotImported ->
                            let
                                ( functionModuleName, functionName ) =
                                    getFunctionModuleAndName fullName

                                moduleAlias : String
                                moduleAlias =
                                    Node.value node
                                        |> .moduleAlias
                                        |> Maybe.map Node.value
                                        |> Maybe.withDefault functionModuleName
                                        |> String.join "."

                                qualifiedName : String
                                qualifiedName =
                                    moduleAlias ++ "." ++ functionName
                            in
                            if (Node.value node |> .moduleName |> Node.value) == functionModuleName then
                                case Node.value node |> .exposingList |> Maybe.map Node.value of
                                    Just (Exposing.All _) ->
                                        { existingImportStatus
                                            | importStatus = FunctionWasImportedExplicitly
                                            , qualifiedName = functionName
                                        }

                                    Just (Exposing.Explicit exposedFunctions) ->
                                        let
                                            isForbiddenFunction : Node TopLevelExpose -> Bool
                                            isForbiddenFunction exposeNode =
                                                case Node.value exposeNode of
                                                    Exposing.FunctionExpose exposedFunction ->
                                                        exposedFunction == functionName

                                                    _ ->
                                                        False
                                        in
                                        if List.any isForbiddenFunction exposedFunctions then
                                            { existingImportStatus
                                                | importStatus = FunctionWasImportedExplicitly
                                                , qualifiedName = functionName
                                            }

                                        else
                                            { existingImportStatus
                                                | importStatus = FunctionWasImported
                                                , qualifiedName = qualifiedName
                                            }

                                    Nothing ->
                                        { existingImportStatus
                                            | importStatus = FunctionWasImported
                                            , qualifiedName = qualifiedName
                                        }

                            else
                                existingImportStatus
            in
            ( []
            , List.map makeImportStatus forbiddenFunctionsStatus
                |> WithForbiddenFunctions
            )


expressionVisitor : List ( List String, List String ) -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor config node context =
    case ( context, Node.value node ) of
        ( WithForbiddenFunctions forbiddenFunctionsStatus, Expression.FunctionOrValue moduleName functionName ) ->
            let
                makeError : ForbiddenFunction -> Maybe (Rule.Error {})
                makeError { importStatus, fullName, qualifiedName } =
                    let
                        ( qualifiedFunctionModule, forbiddenFunctionName ) =
                            getFunctionModuleAndName qualifiedName

                        isFromModule : Bool
                        isFromModule =
                            case importStatus of
                                FunctionWasNotImported ->
                                    False

                                FunctionWasImported ->
                                    moduleName == qualifiedFunctionModule

                                FunctionWasImportedExplicitly ->
                                    moduleName == []

                        isForbiddenFunction : Bool
                        isForbiddenFunction =
                            forbiddenFunctionName == functionName
                    in
                    if isFromModule && isForbiddenFunction then
                        let
                            allowedModules : List String
                            allowedModules =
                                List.foldr
                                    (\( forbiddenFunctions, moduleWhitelist ) currentAllowedModules ->
                                        if List.member fullName forbiddenFunctions then
                                            moduleWhitelist ++ currentAllowedModules

                                        else
                                            currentAllowedModules
                                    )
                                    []
                                    config
                                    |> Set.fromList
                                    |> Set.toList
                        in
                        Node.range node
                            |> Rule.error
                                { message = "You're using the `" ++ fullName ++ "` function outside of the allowed modules"
                                , details =
                                    [ "The `"
                                        ++ fullName
                                        ++ "` function is only allowed to be used in these modules:\n\n"
                                        ++ (List.map (\allowedModule -> "\t`" ++ allowedModule ++ "`") allowedModules
                                                |> String.join "\n"
                                           )
                                    ]
                                }
                            |> Just

                    else
                        Nothing
            in
            ( List.filterMap makeError forbiddenFunctionsStatus
            , context
            )

        _ ->
            ( [], context )
