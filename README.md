# pandoc-markdown-ghci-filter

A Pandoc filter that identifies code blocks(`Haskell`), executes the code in GHCI and embeds the results in the returned Markdown.

# Quick Overview

Often a markdown(or any `pandoc` supported document) for any `Haskell` related documentation or a technical blog post involves `code` blocks. The `code` block could include `definitions` and also a demonstration of output with an `interactive` prompt. For, example, take this `code` block:

``` haskell
-- README.md

-- definition
increment:: Integer -> Integer
increment x = x + 1

-- interactive prompt to demostrate the working of definitions so far

>> increment 41
```

It would be nice if this `code` block was automatically evaluated and `output` of `increment 41` is automatically recorded below `>> increment 41`, as follows:

``` haskell
-- README.md

-- definition
increment:: Integer -> Integer
increment x = x + 1

-- interactive prompt to demostrate the working of definitions so far

>> increment 41
42
```

Notice, that the `42` is automatically populated by this filter while transforming the original document.

To transform the document, we need to run the document through the `pandoc` filter, as follows:

``` shell

-- set up pandoc_filter to the executable of this program (see Installation)

pandoc -s -t json README.md | pandoc_filter | pandoc -f json -t markdown

```

# Installation

``` shell

cd your_folder
git clone https://github.com/gdevanla/pandoc-markdown-ghci-filter.git
cd your_folder/pandoc-markdown-ghci-filter

stack build pandoc-markdown-ghci-filter

# create a link to that executable depending on your OS

# test it on a test_markdown file

pandoc -s -t json README.md | pandoc-markdown-ghci-filter | pandoc -f json -t markdown

```

# Usage Notes/Caveats

1. All interactive statements (prefixed with `>>`) need to be preceded by `\n` to let the filter respect original new line spacing. If this is not followed, `\n` may be truncated.
2. The program internally wraps all commands inside the GHCi multi-line contruct `:{..:}`. Therefore, the code segments should not have multiline constructs as part of code blocks.
3. If you want the filter to ignore a certain `code` block, you can turn-off the filter by setting the `code` block attribute as follows

``` {.haskell code_filter=Off}

-- do not run this code through GHCi

>> putStrLn "This line will not be expanded by the filter"
```

# Limitations/Open Issues

1. Attaching different formattting properties to `output`.
2. As explained in `Usage Notes`, all `interactive` statements should be preceded by an empty line, for the filter to maintain the `\n` characters as given by the input.
