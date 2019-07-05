{-# LANGUAGE OverloadedStrings #-}
module Main where

--import GHC.Generics
import Text.Pandoc
import Text.Pandoc.JSON
import Language.Haskell.Ghcid
import Control.Applicative
import Control.Monad.Fix

--Import Data.Aeson as A
import qualified Data.Text as T

testBlocks :: [Block]
testBlocks = [
  CodeBlock ("",["haskell"],[]) ">> putStrLn \"This string should show up in the output\"\n",
  CodeBlock ("",["haskell"],[]) "testFunc:: Integer -> Integer\ntestFunc x = x + 1\n\n>> testFunc 13\n",
  CodeBlock ("",["haskell"],[]) "testFunc:: Integer -> Integer\ntestFunc x = x + 1\n\n>> testFunc 13\n\ntestFunc:: Integer -> Integer\ntestFunc x = x + 1\n",
  CodeBlock ("",["haskell"],[]) "testFunc1:: Integer -> Integer\ntestFunc1 x = x + 1\n\n>> testFunc1 13\n\ntestFunc2:: Integer -> Integer\ntestFunc2 x = x + 1\n\n>> testFunc2 5\n"
  ]


--replaceAll pat str = if (T.replace pat str) == str then str else replaceAll pat (T.replace pat str)

replaceAll' f (pat, str) = if ((T.replace pat "" str) == str) then str
  else f (pat, T.replace pat "" str)

ghcid_pattern =  "*Main Lib INTERNAL_GHCID| "

interactive str = (T.take 2 str) == ">>"

trail_interactive str = if interactive str then
  if T.last str == '\n' then "" else "\n"
  else "\n\n"


runCodeBlock:: Block -> IO Block
runCodeBlock (CodeBlock attr str) = do
  (g, _) <- startGhci "stack ghci" (Just ".") (\stream s -> return ())
  let cmds = filter (\s -> s /= "") $ T.splitOn "\n\n" $ T.pack str
  results <- mapM (runCmd g) cmds
  let results' = getZipList $ (\s t -> T.concat [s, trail_interactive s, t, if t /= "" then "\n" else ""]) <$> ZipList cmds <*> ZipList results
  --putStrLn . show . getZipList $ results_
  --'putStrLn . show $ results'
  let results'' = map ((fix replaceAll')) [(ghcid_pattern, x)|x<-results']
  let results''' = map (\s -> T.concat [s, ""]) results''
  stopGhci g
  return (CodeBlock attr ((T.unpack . T.concat) results'''))
runCodeBlock b = return b

--runCmd :: T.Text -> IO T.Text
runCmd g cmd = do
  -- (g, _) <- startGhci "stack ghci" (Just ".") (\stream s -> return ())
  let executeStatement = exec g
      cmd_ = T.concat [":{\n", T.replace ">>" "" cmd, "\n:}\n"]
  --putStrLn $ T.unpack cmd_
  result <- executeStatement . T.unpack $ cmd_
  --putStrLn $ unlines $ result
  return $ T.pack . unlines $ result

-- main :: IO ()
-- main = do
--   result <- mapM runCodeBlock testBlocks
--   putStrLn $ show $ result
--   return ()

main :: IO ()
main = toJSONFilter runCodeBlock
--   where behead (Header n _ xs) | n >= 2 = return $ Para [Emph xs]
--         behead z@(CodeBlock attr str) = updateCodeBlock z



--     (g, _) <- startGhci "stack ghci" (Just ".") (\stream s -> return ())
--     let executeStatement = exec g
--     executeStatement "data Person = Person {name:: String} deriving (Show)" >>= print
--     executeStatement "let x = Person \"name\"">>= print
--     executeStatement "print x" >>= print . head
--     executeStatement cmd >>= print
--     executeStatement "print $ x1 10 20" >>= print . head
--     stopGhci g


-- updateCodeBlock:: Block-> IO Block
-- updateCodeBlock code = do
--   putStrLn $ show code
--   return code


-- cmd = unlines [
--   ":{",
--   "    let x1:: Int -> Int -> Int",
--   "        x1 a b = a + b",
--   ":}"
--   ]

-- main :: IO ()
-- main = do
--     (g, _) <- startGhci "stack ghci" (Just ".") (\stream s -> return ())
--     let executeStatement = exec g
--     executeStatement "data Person = Person {name:: String} deriving (Show)" >>= print
--     executeStatement "let x = Person \"name\"">>= print
--     executeStatement "print x" >>= print . head
--     executeStatement cmd >>= print
--     executeStatement "print $ x1 10 20" >>= print . head
--     stopGhci g



-- data Person = Person  {
--    name :: T.Text
--   ,age :: Int
--   } deriving (Generic, Show)


-- -- instance A.ToJSON Person where
-- --   toEncoding = genericToEncoding A.defaultOptions

-- instance FromJSON Person

-- data NewPeprson = NewPerson  {
--    new_name :: T.Text
--   , new_age :: Int
--   } deriving (Show)


-- instance FromJSON NewPerson where
--   parseJSON = withObject "NewPerson" $ \v -> NewPerson
--     <$> v .: "new_name"
--     <*> v .: "new_age"

-- instance ToJSON Person where
--   toJSON (Person name1 age1) =
--     object ["name" .= name1, "age" .= age1]

--   toEncoding (Person name1 age1) =
--     pairs ("name" .= name1 <> "age" .= age1)
