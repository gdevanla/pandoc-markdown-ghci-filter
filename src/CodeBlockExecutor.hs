{-# LANGUAGE OverloadedStrings #-}

module CodeBlockExecutor
    ( runCodeBlock
    ) where

--import GHC.Generics
import Text.Pandoc

import Language.Haskell.Ghcid
import Control.Applicative
import Control.Exception
import Data.String

import qualified Data.Text as T

removeAll:: T.Text -> T.Text -> T.Text
removeAll pat str = if (T.replace pat "" str) == str then str
  else removeAll pat (T.replace pat "" str)

ghcid_pattern :: T.Text
ghcid_pattern =  "*Main CodeBlockExecutor INTERNAL_GHCID| "

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

processResults :: [T.Text] -> [T.Text] -> [T.Text]
processResults cmds results =
  let cmd_result = getZipList $ intercalateCmdAndResults <$> ZipList cmds <*> ZipList results
  in
    map (removeAll ghcid_pattern) cmd_result

runCodeBlock:: Block -> IO Block
runCodeBlock (CodeBlock attr str) = bracket startGhciProcess' stopGhci runCommands
  where
    startGhciProcess' = do
      (ghci_handle, _) <- startGhci "stack ghci" (Just ".") (\_ _ -> return ())
      return ghci_handle
    runCommands g = do
      let cmds = filter (\s -> s /= "") $ T.splitOn "\n\n" $ T.pack str
      results <- mapM (runCmd g) cmds
      let results''' = processResults cmds results
      return (CodeBlock attr ((T.unpack . T.concat) results'''))
runCodeBlock b = return b

runCmd :: Ghci -> T.Text -> IO T.Text
runCmd g cmd = do
  let executeStatement = exec g
      cmd_ = T.concat [":{\n", T.replace ">>" "" cmd, "\n:}\n"]
  result <- executeStatement . T.unpack $ cmd_
  return $ T.pack . unlines $ result
