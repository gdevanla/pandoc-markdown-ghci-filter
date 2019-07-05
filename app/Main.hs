module Main where

import Text.Pandoc.JSON
import CodeBlockExecutor

main :: IO ()
main = toJSONFilter runCodeBlock
