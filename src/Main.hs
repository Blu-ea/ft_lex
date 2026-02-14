module Main (main) where

import System.Environment ( getArgs )
import Control.Applicative ( Alternative(some) )

import ProgOption ( Options(Options), processOpts, printUsage )
import ParserDef.InputLex ( getInput, lexParse )
import ParserDef.Parser ( Parser(runParser) )
import GHC.IO
import Data.Either (fromRight)


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
    -- let a = runParser ((blankLines *> sectionSeparator) *> ruleParse <* (blankLines *> sectionSeparator)) t
    -- if isLeft a then
    --     print a
    -- else do
    --     putStrLn ""
    --     printList . fst $ fromRight a
    --     putStrLn ""
    --     printList . snd $ fromRight a
    --     putStrLn ""
    
    putStrLn "\n\n\n"
    case a of
        Left a -> putStrLn a
        Right a -> putStrLn $ show a
    return ()
