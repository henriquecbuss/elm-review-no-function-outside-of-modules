# elm-review-no-function-outside-of-modules

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`NoFunctionOutsideOfModules`](https://package.elm-lang.org/packages/NeoVier/elm-review-no-function-outside-of-modules/1.0.0/NoFunctionOutsideOfModules) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoFunctionOutsideOfModules
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoFunctionOutsideOfModules.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template NeoVier/elm-review-no-function-outside-of-modules/example
```
