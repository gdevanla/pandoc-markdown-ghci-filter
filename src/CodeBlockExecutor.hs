{-# LANGUAGE OverloadedStrings #-}

-- | Module that communicates with GHCid, splits the commands in the code block,
-- gather the results and emits the trasnformed JSON based Pandoc AST.


module CodeBlockExecutor
    (
      applyFilterToBlock,
      runCodeBlock,
      processResults
    ) where


--import GHC.Generics
import Text.Pandoc

import Language.Haskell.Ghcid
import Control.Applicative
import Control.Exception
import Data.String
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe

import qualified Data.Text as T

removeAll:: T.Text -> T.Text -> T.Text
removeAll pat str = if (T.replace pat "" str) == str then str
  else removeAll pat (T.replace pat "" str)

-- | Determines if the command encountered is an `interactive` command.
-- This is one of the conditions that determines whether output needs to be captured.
isInteractive :: T.Text -> Bool
isInteractive cmd = T.isPrefixOf ">>" cmd

-- | This function takes care of placing back the newline characters
-- that may been stripped out before sending the commands to GHCid
updateSuffixForInteractiveCmd :: Data.String.IsString p => T.Text -> p
updateSuffixForInteractiveCmd cmd = if isInteractive cmd then
  if T.last cmd == '\n' then "" else "\n"
  else "\n\n"

-- | Intercalate the commands and respective results. Typically, only errors
-- encountered while running definitions, and outut of interactive commands (
-- i.e commands prefixed with `>>`) are captured. All empty strins are dropped.
intercalateCmdAndResults :: T.Text -> T.Text -> T.Text
intercalateCmdAndResults cmd result =
  T.concat [cmd, updateSuffixForInteractiveCmd cmd, result, trailResult result] where
  trailResult r = if r /= "" then "\n" else ""

-- | Post-processing function that interleaves command and results
processResults :: [T.Text] -- ^ List of commands that were executed
               -> [T.Text] -- ^ List of results for the executed commands
               -> String -- ^ New string that will form the bodyof the modified Code Block.
processResults cmds results =
  let cmd_result = getZipList $ intercalateCmdAndResults <$> ZipList cmds <*> ZipList results
  in
    (T.unpack . T.concat) $ cmd_result

-- | Apply the filter block only if the language attribute
-- is set to `haskell` and `code-filter` property is *not* set to "Off"
--
-- Example:
-- ``` {.haskell code-filter="Off"} ``` will turn off any kind of transformation.
--
-- By default the filer is "On"
applyFilterToBlock:: Block -- ^ The 'Block' yielded by toJSONFilter in "Text.Pandoc.JSON"
                  -> IO Block -- ^ The newly minted 'Block'
applyFilterToBlock c@(CodeBlock (_, classes, key_values) _) = let
  attrs = M.fromList key_values
  haskell_in_class = L.find (== "haskell") classes
  code_filter_flag = maybe "On" id (M.lookup ("code-filter") attrs)
  in
    if code_filter_flag == "On" && isJust haskell_in_class then runCodeBlock c
    else (return c)
applyFilterToBlock b = return b

-- | Run the commands in the 'Block' in one single GHCid session.
runCodeBlock:: Block -- ^ 'Block' to execute. Only 'CodeBlock' is executed.
            -> IO Block -- ^ transformed code block
runCodeBlock (CodeBlock attr str) = bracket startGhciProcess' stopGhci runCommands
  where
    startGhciProcess' = do
      (ghci_handle, _) <- startGhci "stack ghci" (Just ".") (\_ _ -> return ())
      return ghci_handle
    runCommands g = do
      let cmds = L.filter (\s -> s /= "") $ T.splitOn "\n\n" $ T.pack str
      results <- mapM (runCmd g) cmds
      let results''' = processResults cmds results
      return (CodeBlock attr results''')
runCodeBlock b = return b

-- | Executes a command in GHCIid.
runCmd :: Ghci -- ^ Handle to the GHCi process through the GHCid interface
       -> T.Text -- ^ Statement to execute
       -> IO T.Text -- ^ Result of the executed statement
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
