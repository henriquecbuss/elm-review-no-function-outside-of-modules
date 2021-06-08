module NoFunctionOutsideOfModulesTest exposing (all)

import NoFunctionOutsideOfModules exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    let
        defaultRule =
            rule [ ( "Html.input", [ "View.Input" ] ) ]
    in
    describe "NoFunctionOutsideOfModules"
        [ test "should report an error when using a qualified name outside of module" <|
            \() ->
                """module Main exposing (main)

import Html

main : Html.Html a
main =
    Html.input [] []
"""
                    |> Review.Test.run defaultRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\t`View.Input`" ]
                            , under = "Html.input [] []"
                            }
                        ]
        , test "should report an error when using an exposed name outside of module" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input)

main : Html.Html a
main =
    input [] []
"""
                    |> Review.Test.run defaultRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 10 } }
                        ]
        , test "should be successful when using the function inside module" <|
            \() ->
                """module View.Input exposing (customInput)

import Html

customInput : Html.Html a
customInput =
    Html.input [] []
"""
                    |> Review.Test.run defaultRule
                    |> Review.Test.expectNoErrors
        ]
