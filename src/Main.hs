module Main where

import Data.List (isSuffixOf)
import Parser
import ParsingLang
import System.Environment
import System.Exit

replaceSuffix :: String -> String -> String -> String
replaceSuffix suffix suffix' str =
  if suffix `isSuffixOf` str then take d str ++ suffix' else str ++ suffix'
  where
    d = length str - length suffix

removeFolders :: String -> String
removeFolders = reverse . takeWhile ('/' /=) . reverse

getOutName :: Maybe String -> String -> String
getOutName Nothing = replaceSuffix ".sch" ".js"
getOutName (Just folder) = ((folder ++ "/") ++) . removeFolders . replaceSuffix ".sch" ".js"

treatFile :: Maybe String -> String -> IO ()
treatFile folder filename =
  do
    file <- readFile filename
    writeFile (getOutName folder filename) $ case schemastoAJV . snd <$> runParser parseSchemas file of Just u -> u
    putStrLn $ getOutName Nothing filename ++ " done"

main :: IO [()]
main = getArgs >>= folderArg >>= treatFiles
  where
    folderArg :: [String] -> IO (Maybe String, [String])
    folderArg ("-o" : folder : rest) = return (Just folder, rest)
    folderArg rest = return (Nothing, rest)
    treatFiles :: (Maybe String, [String]) -> IO [()]
    treatFiles (folder, filenames) = mapM (treatFile folder) filenames
