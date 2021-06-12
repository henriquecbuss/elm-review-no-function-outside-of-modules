# elm-review-no-function-outside-of-modules

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to forbid using functions outside of certain modules.

## Provided rules

- [`NoFunctionOutsideOfModules`](https://package.elm-lang.org/packages/NeoVier/elm-review-no-function-outside-of-modules/2.0.0/NoFunctionOutsideOfModules) - Reports uses of certain functions outside of certain modules.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoFunctionOutsideOfModules
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoFunctionOutsideOfModules.rule [ ( [ "Html.input" ], [ "View.Input" ] ) ]
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template NeoVier/elm-review-no-function-outside-of-modules/example-with-no-html-input-outside-of-view
```
