module NoFunctionOutsideOfModulesTest exposing (all)

import NoFunctionOutsideOfModules exposing (rule)
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    let
        htmlRule : Rule
        htmlRule =
            rule [ ( [ "Html.input" ], [ "View.Input" ] ) ]

        multipleHtmlRule : Rule
        multipleHtmlRule =
            rule [ ( [ "Html.input", "Html.textarea" ], [ "View.Input" ] ) ]

        fruitRule : Rule
        fruitRule =
            rule [ ( [ "Json.Encode.object" ], [ "Fruits.Json" ] ) ]
    in
    describe "NoFunctionOutsideOfModules"
        [ test "should report an error when using Html.input outside of module" <|
            \() ->
                """module Main exposing (main)

import Html

main : Html.Html a
main =
    Html.input [] []
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.input"
                            }
                        ]
        , test "should report an error when using Html.input outside of module and inside other elements" <|
            \() ->
                """module Main exposing (main)

import Html

main : Html.Html a
main =
    Html.div [] [ Html.input [] [] ]
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.input"
                            }
                        ]
        , test "should report an error when using an exposed name (Html.input) outside of module and inside other elements" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input)

main : Html.Html a
main =
    Html.div [] [ input [] [] ]
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 19 }, end = { row = 7, column = 24 } }
                        ]
        , test "should report an error when using an exposed name (Html.input) outside of module" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input)

main : Html.Html a
main =
    input [] []
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 10 } }
                        ]
        , test "should report an error when using qualified name with exposed `Html.input`" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input)
import SomeOtherView

main : Html.Html a
main =
    input [] []
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 10 } }
                        ]
        , test "should report an error when using Json.Encode.object outside of module" <|
            \() ->
                """module Fruits exposing (encodedFruits)

import Json.Encode

encodedFruits : Json.Encode.Value
encodedFruits =
    Json.Encode.object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Json.Encode.object` function outside of the allowed modules"
                            , details = [ "The `Json.Encode.object` function is only allowed to be used in these modules:\n\n\t`Fruits.Json`" ]
                            , under = "Json.Encode.object"
                            }
                        ]
        , test "should report an error when using Encode.object outside of module" <|
            \() ->
                """module Fruits exposing (encodedFruits)

import Json.Encode as Encode

encodedFruits : Encode.Value
encodedFruits =
    Encode.object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Json.Encode.object` function outside of the allowed modules"
                            , details = [ "The `Json.Encode.object` function is only allowed to be used in these modules:\n\n\t`Fruits.Json`" ]
                            , under = "Encode.object"
                            }
                        ]
        , test "should report an error when using an exposed name (Json.Encode.value) outside of module" <|
            \() ->
                """module Fruits exposing (encodedFruits)

import Json.Encode as Encode exposing (object)

encodedFruits : Json.Encode.Value
encodedFruits =
    object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Json.Encode.object` function outside of the allowed modules"
                            , details = [ "The `Json.Encode.object` function is only allowed to be used in these modules:\n\n\t`Fruits.Json`" ]
                            , under = "object"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 11 } }
                        ]
        , test "should report two errors when using Html.input and Html.textarea outside of module" <|
            \() ->
                """module Main exposing (main)

import Html

main : Html.Html a
main =
    Html.div []
        [ Html.input [] []
        , Html.textarea [] []
        ]
"""
                    |> Review.Test.run multipleHtmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.input"
                            }
                        , Review.Test.error
                            { message = "You're using the `Html.textarea` function outside of the allowed modules"
                            , details = [ "The `Html.textarea` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.textarea"
                            }
                        ]
        , test "should report two errors when using exposed Html.input and exposed Html.textarea outside of module" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input, textarea)

main : Html.Html a
main =
    Html.div []
        [ input [] []
        , textarea [] []
        ]
"""
                    |> Review.Test.run multipleHtmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 11 }, end = { row = 8, column = 16 } }
                        , Review.Test.error
                            { message = "You're using the `Html.textarea` function outside of the allowed modules"
                            , details = [ "The `Html.textarea` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "textarea"
                            }
                            |> Review.Test.atExactly { start = { row = 9, column = 11 }, end = { row = 9, column = 19 } }
                        ]
        , test "should report two errors when using exposed Html.input and Html.textarea outside of module" <|
            \() ->
                """module Main exposing (main)

import Html exposing (input)

main : Html.Html a
main =
    Html.div []
        [ input [] []
        , Html.textarea [] []
        ]
"""
                    |> Review.Test.run multipleHtmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.input` function outside of the allowed modules"
                            , details = [ "The `Html.input` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "input"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 11 }, end = { row = 8, column = 16 } }
                        , Review.Test.error
                            { message = "You're using the `Html.textarea` function outside of the allowed modules"
                            , details = [ "The `Html.textarea` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.textarea"
                            }
                        ]
        , test "should report one error when using Html.textarea outside of module" <|
            \() ->
                """module Main exposing (main)

import Html

main : Html.Html a
main =
    Html.div [] [ Html.textarea [] [] ]
"""
                    |> Review.Test.run multipleHtmlRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You're using the `Html.textarea` function outside of the allowed modules"
                            , details = [ "The `Html.textarea` function is only allowed to be used in these modules:\n\n\t`View.Input`" ]
                            , under = "Html.textarea"
                            }
                        ]
        , test "should be successful when using Html.input inside module" <|
            \() ->
                """module View.Input exposing (customInput)

import Html

customInput : Html.Html a
customInput =
    Html.input [] []
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectNoErrors
        , test "should be successful when using other module's `input` when exposing `Html.input`" <|
            \() ->
                """module CustomInput exposing (customInput)

import Html exposing (input)
import SomeViewModule

customInput : Html.Html a
customInput =
    SomeViewModule.input [] []
"""
                    |> Review.Test.run htmlRule
                    |> Review.Test.expectNoErrors
        , test "should be successful when using the fruit encoding function inside module" <|
            \() ->
                """module Fruits.Json exposing (encodedFruits)

import Json.Encode

encodedFruits : Json.Encode.Value
encodedFruits =
    Json.Encode.object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectNoErrors
        , test "should be successful when using the fruit encoding function inside module with qualified import" <|
            \() ->
                """module Fruits.Json exposing (encodedFruits)
import Json.Encode as Encode

encodedFruits : Encode.Value
encodedFruits =
    Encode.object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectNoErrors
        , test "should be successful when using the fruit encoding function inside module with exposed function import" <|
            \() ->
                """module Fruits.Json exposing (encodedFruits)
import Json.Encode exposing (object)

encodedFruits : Json.Encode.Value
encodedFruits =
    object [ ( "apple", "🍎" ), ( "pineapple", "🍍" ) ]
"""
                    |> Review.Test.run fruitRule
                    |> Review.Test.expectNoErrors
        , test "should be successful when using Html.input and Html.textarea inside of module" <|
            \() ->
                """module View.Input exposing (main)

import Html

main : Html.Html a
main =
    Html.div []
        [ Html.input [] []
        , Html.textarea [] []
        ]
"""
                    |> Review.Test.run multipleHtmlRule
                    |> Review.Test.expectNoErrors
        ]
