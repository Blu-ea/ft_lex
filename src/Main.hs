module Main (main) where

import System.Environment ( getArgs )
import Control.Applicative ( Alternative(some) )
import GHC.IO

import ProgOption ( Options(Options), processOpts, printUsage )
import InputDef.InputParse ( getInput, lexParse )
import Parser ( Parser(runParser) )
import InputDef.LexDefinition ( LexFile(LexFile), Rule(RRule) ) 
import RegexEngine.StringToRegex (translateRegex)
import Data.Tuple


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
    a <- evaluate $ runParser lexParse t

    putStrLn "\n\n\n"
    case a of
        Left a' -> print a'
        Right a'@(LexFile defs rules usr, _) -> do 
            print a'
            print . map (\r -> translateRegex (getString r) defs) $ filter isRRule rules

    
    return ()

    where 
        isRRule (RRule _ _)= True 
        isRRule _ = False
        getString (RRule r _) = r