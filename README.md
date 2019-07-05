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

# Installtion

``` shell

cd your_folder
git clone https://github.com/gdevanla/pandoc-markdown-ghci-filter.git
cd your_folder/pandoc-markdown-ghci-filter

stack build

# find build folder using the following command
stack path --dist-dir

# create a link to that executable depending on your OS

# test it on a test_markdown file

pandoc -s -t json README.md | pandoc_filter | pandoc -f json -t markdown

```
