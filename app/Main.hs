{-# LANGUAGE OverloadedStrings #-}
module Main where

--import GHC.Generics
import Text.Pandoc
import Text.Pandoc.JSON
import Language.Haskell.Ghcid
import Control.Applicative
import Data.String

import qualified Data.Text as T

-- testBlocks :: [Block]
-- testBlocks = [
--   CodeBlock ("",["haskell"],[]) ">> putStrLn \"This string should show up in the output\"\n",
--   CodeBlock ("",["haskell"],[]) "testFunc:: Integer -> Integer\ntestFunc x = x + 1\n\n>> testFunc 13\n",
--   CodeBlock ("",["haskell"],[]) "testFunc:: Integer -> Integer\ntestFunc x = x + 1\n\n>> testFunc 13\n\ntestFunc:: Integer -> Integer\ntestFunc x = x + 1\n",
--   CodeBlock ("",["haskell"],[]) "testFunc1:: Integer -> Integer\ntestFunc1 x = x + 1\n\n>> testFunc1 13\n\ntestFunc2:: Integer -> Integer\ntestFunc2 x = x + 1\n\n>> testFunc2 5\n"
--   ]

removeAll:: T.Text -> T.Text -> T.Text
removeAll pat str = if (T.replace pat "" str) == str then str
  else removeAll pat (T.replace pat "" str)

ghcid_pattern :: T.Text
ghcid_pattern =  "*Main Lib INTERNAL_GHCID| "

isInteractive :: T.Text -> Bool
isInteractive cmd = T.isPrefixOf ">>" cmd

updateSuffixForInteractiveCmd :: Data.String.IsString p => T.Text -> p
updateSuffixForInteractiveCmd cmd = if isInteractive cmd then
  if T.last cmd == '\n' then "" else "\n"
  else "\n\n"

intercalateCmdAndResults :: T.Text -> T.Text -> T.Text
intercalateCmdAndResults cmd result =
  T.concat [cmd, updateSuffixForInteractiveCmd cmd, result, trailResult result] where
  trailResult r = if r /= "" then "\n" else ""

runCodeBlock:: Block -> IO Block
runCodeBlock (CodeBlock attr str) = do
  (g, _) <- startGhci "stack ghci" (Just ".") (\_ _ -> return ())
  let cmds = filter (\s -> s /= "") $ T.splitOn "\n\n" $ T.pack str
  results <- mapM (runCmd g) cmds
  let results' = getZipList $ intercalateCmdAndResults <$> ZipList cmds <*> ZipList results
  let results'' = map (removeAll ghcid_pattern) results'
  let results''' = map (\s -> T.concat [s, ""]) results''
  stopGhci g
  return (CodeBlock attr ((T.unpack . T.concat) results'''))
runCodeBlock b = return b

runCmd :: Ghci -> T.Text -> IO T.Text
runCmd g cmd = do
  let executeStatement = exec g
      cmd_ = T.concat [":{\n", T.replace ">>" "" cmd, "\n:}\n"]
  result <- executeStatement . T.unpack $ cmd_

  return $ T.pack . unlines $ result


main :: IO ()
main = toJSONFilter runCodeBlock
