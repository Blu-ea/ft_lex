module Main (main) where

import ProgOption ( Options(Options), processOpts, printUsage )
import System.Environment (getArgs)
import InputLex
import Data.Maybe
import Control.Applicative


main :: IO ()
main = do
    res <- processOpts <$> getArgs
    case res of
        Left errStr -> putStr errStr
        Right (opt, stropt) -> do
            runProgram opt stropt

runProgram :: Options -> [FilePath] -> IO()
runProgram (Options _ _ True) _ = printUsage
runProgram _ input = do
    t <- getInput input
    let a = runParser (many defParse) t
    print . fst $ fromJust a
    print . snd $ fromJust a
    return ()
