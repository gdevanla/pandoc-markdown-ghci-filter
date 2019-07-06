## Sample Markdown Before Transformation

Sample markdown as fed into filter through `pandoc`.

## Example 1

``` {.haskell code-filter=Off}
import Data.Text

>> putStrLn "This string should show up in the output"

```
## Example 2

``` {.haskell code-filter=Off}
addOne:: Integer -> Integer
addOne x = x + 1

>> addOne 13

multBy2:: Integer -> Integer
multBy = x * 2

>> (addOne 10) + (multBy2 20)
```

## Markdown after transformation

``` {.haskell code-filter=On}
import Data.Text

>> putStrLn "This string should show up in the output"

```
## Example 2

``` {.haskell code-filter=On}
addOne:: Integer -> Integer
addOne x = x + 1

>> addOne 13

multBy2:: Integer -> Integer
multBy = x * 2

>> (addOne 10) + (multBy2 20)
```
