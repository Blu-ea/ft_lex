module Main (main) where

import System.Environment ( getArgs )
import Control.Applicative ( Alternative(some) )

import ProgOption ( Options(Options), processOpts, printUsage )
import ParserDef.InputLex ( getInput, lexParse )
import ParserDef.Parser ( Parser(runParser) )
import GHC.IO


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
    a <- evaluate $ runParser (some lexParse) t

    putStrLn "\n\n\n"
    case a of
        Left a' -> print a'
        Right a' -> print a'
    return ()
