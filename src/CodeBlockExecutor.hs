{-# LANGUAGE OverloadedStrings #-}

module CodeBlockExecutor
    ( runCodeBlock,
      processResults
    ) where

--import GHC.Generics
import Text.Pandoc

import Language.Haskell.Ghcid
import Control.Applicative
import Control.Exception
import Data.String
import Data.List as L

import qualified Data.Text as T

removeAll:: T.Text -> T.Text -> T.Text
removeAll pat str = if (T.replace pat "" str) == str then str
  else removeAll pat (T.replace pat "" str)

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

processResults :: [T.Text] -> [T.Text] -> String
processResults cmds results =
  let cmd_result = getZipList $ intercalateCmdAndResults <$> ZipList cmds <*> ZipList results
  in
    (T.unpack . T.concat) $ cmd_result

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
      return (CodeBlock attr results''')
runCodeBlock b = return b

runCmd :: Ghci -> T.Text -> IO T.Text
runCmd g cmd = do
  let executeStatement = exec g
      cmd_ = T.concat [":{\n", T.replace ">>" "" cmd, "\n:}\n"]
  result <- executeStatement . T.unpack $ cmd_
  -- we send this PROBE here since GHCi has its own mind on how it prefixes output based on its native needs. By sending the probe we can guess what is the latest prompt and then discard it while processing thye output.
  probe <- exec g ":{\nshow (\"PANDOC_FILTER_PROBE_PROMPT_INTERNAL\"::String)\n:}\n"
  let current_prompt = preparePrompt probe
        where
          preparePrompt probe' =
            let prompt = T.replace " \"\\\"PANDOC_FILTER_PROBE_PROMPT_INTERNAL\\\"\"\n" "" (T.pack . unlines $ probe')
            in
              T.concat [T.takeWhile (/='|') prompt, "|"]
  --putStrLn $ show . unlines $ probe
  --putStrLn $ show current_prompt
      result' = T.stripStart $ removeAll current_prompt (T.pack . unlines $ result)
  --putStrLn $ show result'
  return $ result'
