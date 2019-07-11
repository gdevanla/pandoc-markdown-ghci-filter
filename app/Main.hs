module Main where

import Text.Pandoc.JSON
import CodeBlockExecutor

-- test c@(CodeBlock (identifier, classes, key_value) str) = do
--   putStrLn $ show key_value
--   putStrLn $ show classes
--   return c
-- test b = return b

-- main :: IO ()
-- main = toJSONFilter test -- applyFilterToBlock

main :: IO ()
main = toJSONFilter applyFilterToBlock
