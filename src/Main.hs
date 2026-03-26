module Main (main) where

import System.Environment ( getArgs )
import Data.Maybe ( mapMaybe )
import Debug.Trace ( trace )
import Control.Monad (when)
import System.Exit (exitFailure)
import Data.Either (lefts)

import Parser ( Parser(runParser) )
import ProgOption ( Options(Options), processOpts, printUsage )
import InputDef.InputParse ( getInput, lexParse )
import InputDef.LexDefinition ( LexFile(LexFile), Rule(RRule) )
import RegexEngine.StringToRegex ( translateRegex, regexParse )
import InputDef.InputChar ( toString, InputChar (InputChar) )


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
    let a = runParser lexParse t
    case a of
        Left a' -> putStr a'
        Right (LexFile defs rules usr, _) -> do
            let ruleList =
                    map (\s ->
                        case runParser regexParse (fst s) of  
                            Left err -> Left $ (snd . snd $ s) ++ ':' : show (fst . snd $ s) ++ ": " ++ err
                            Right (reg, []) -> Right (reg, snd s)
                            Right (_, _) -> Left $ (snd . snd $ s) ++ ':' : show (fst . snd $ s) ++ ": Unvalide Regex Expression")
                    $ mapMaybe (\r ->
                        case translateRegex (getString r) (getPos r) defs of
                            Left err -> trace (snd (getPos r) ++ ':' : show (fst (getPos r)) ++ ": " ++ err ) Nothing
                            Right res -> Just res)
                    (filter isRRule rules)
            when (not (null $ lefts ruleList) || length ruleList /= length rules ) $ mapM_ putStrLn (lefts ruleList) >> exitFailure
            putStrLn "PASS"
    return ()

    where
        isRRule (RRule {})= True
        isRRule _ = False
        getPos (RRule ((InputChar _ ((x, _), file)):_) _) = (x, file)
        getString (RRule r _) = toString r