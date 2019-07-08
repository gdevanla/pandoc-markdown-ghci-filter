# pandoc-markdown-ghci-filter

A `Pandoc` filter that identifies code blocks(`Haskell`) in Pandoc supported formats, executes the code in GHCI and embeds the results in the returned output by updating the AST provided by `Pandoc`.Note, this library is tested with pandoc 2.7.3 and may not be compatible with older versions.

# Quick Overview

Often a markdown(or any `pandoc` supported document) for any `Haskell` related documentation or a technical blog post involves `code` blocks. The `code` block could include `definitions` and also a demonstration of output with an `interactive` prompt. For, example, take this `code` block:

``` {.haskell code-filter=Off}
-- README.md

-- definition
increment:: Integer -> Integer
increment x = x + 1

-- interactive prompt to demonstrate the working of definitions so far

>> increment 41
```

It would be nice if this `code` block was automatically evaluated and `output` of `increment 41` is automatically recorded below `>> increment 41`, as follows:

``` haskell
-- README.md

-- definition
increment:: Integer -> Integer
increment x = x + 1

-- interactive prompt to demonstrate the working of definitions so far

>> increment 41
```

Notice, that the `42` is automatically populated by this filter while transforming the original document.

To transform the document, we need to run the document through the `pandoc` filter, as follows:

``` shell

-- set up pandoc_filter to the executable of this program (see Installation)

pandoc -s -t json README.md | pandoc_filter | pandoc -f json -t markdown

```

To read more about how `filter` work, visit the [this](https://pandoc.org/filters.html) page.

# Installation

## Requirements

    - [Stack](https://docs.haskellstack.org/en/stable/README/)


### From Source
``` shell

git clone https://github.com/gdevanla/pandoc-markdown-ghci-filter.git
cd pandoc-markdown-ghci-filter

stack build

```

### From Stackage/Hackage

``` shell
stack build pandoc-markdown-ghci-filter # executable only available to local stack environment

or

stack install pandoc-markdown-ghci-filter # if you want to across all stack environments

```

# Running the filter

``` shell
pandoc -s -t json test.md | pandoc-markdown-ghci-filter-exe | pandoc -f json -t markdown
```

# Usage Notes/Caveats

1. All interactive statements (prefixed with `>>`) need to be preceded by `\n` to let the filter respect original new line spacing. If this is not followed, `\n` may be truncated.
2. The program internally wraps all commands inside the GHCi multi-line construct `:{..:}`. Therefore, the code segments should not have multi-line constructs as part of code blocks.
3. If you want the filter to ignore a certain `code` block, you can turn-off the filter by setting the `code` block attribute as follows


``` markdown

{.haskell code_filter="Off"}

-- do not run this code through GHCi

>> putStrLn "This line will not be expanded by the filter"
```

Note, the default value is "On"

# Limitations/Open Issues

1. Attaching different formattting properties to `output`.
2. As explained in `Usage Notes`, all `interactive` statements should be preceded by an empty line, for the filter to maintain the `\n` characters as given by the input.

# More examples

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
multBy2 x = x * 2

>> (addOne 10) + (multBy2 20)
```

## Example 3

Any errors that occur while executing statements in the `code` block are also rendered.

``` {.haskell code-filter=Off}

wrongFuncDefinition:: Integer -> Integer
wrongFuncDefintion = x + 1

>> functionNotInScope 10
```

## Example 4

Any errors that occur while executing statements in the `code` block are also rendered.

``` {.haskell code-filter=Off}

testDefinition :: Integer -> Integer -> Integer
testDefinition x y = x + y

>>:t testDefinition

>>:t testDefinition 10

>>:t testDefinition 10 20

```



## Markdown after transformation

## Example 1

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
multBy2 x = x * 2

>> (addOne 10) + (multBy2 20)
```

## Example 3

Any errors that occur while executing statements in the `code` block are also rendered.

``` {.haskell code-filter=On}

wrongFuncDefinition:: Integer -> Integer
wrongFuncDefintion = x + 1

>> functionNotInScope 10
```

## Example 4

Any errors that occur while executing statements in the `code` block are also rendered.

``` {.haskell code-filter=On}

testDefinition :: Integer -> Integer -> Integer
testDefinition x y = x + y

>>:t testDefinition

>>:t testDefinition 10

>>:t testDefinition 10 20

```


*Fun Fact:* This document was generated using this same tool it describes. This [README-pre-process.md](https://github.com/gdevanla/pandoc-markdown-ghci-filter/blob/master/README-pre-process.md) was used to generate this document. Here is the command that was used:

``` shell
pandoc -s -t json README-pre-process.md | stack runhaskell app/Main.hs | pandoc -f json -t markdown > README.md
```
