module ReviewConfig exposing (config)

import NoFunctionOutsideOfModules
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoFunctionOutsideOfModules.rule "Html.input" [ "View.Input" ]
    ]
