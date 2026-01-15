module Main (main) where

import ProgOption (processOpts)
import System.Environment (getArgs)
import System.IO ( openFile , hGetContents)
import GHC.Read (readField)

main :: IO ()
main = do
    res <- processOpts <$> getArgs
    case res of
        Left errStr -> putStrLn errStr
        Right (opt, stropt) -> do
            print opt
            getInput stropt >>= putStrLn
            return ()


getInput :: [FilePath] -> IO String
getInput [] = getContents
getInput args = concat <$> mapM readFile args
