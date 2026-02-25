module RegexEngine.StringToRegex where

import InputDef.LexDefinition ( Definition, getDefinition)
import Parser
import RegexEngine.RegexTokenType

import GHC.Unicode
import Control.Applicative (Alternative(many))
import InputDef.InputParse (lookAhead)
import Data.Char
import Debug.Trace

translateRegex :: String -> [Definition] -> Maybe String
translateRegex [] _ = Just []
translateRegex ('{':rs) env =
    let (word, rss) = search rs in

    if not (null word) && isAlpha (head word) && all isAlphaNum (tail word) then do
        translated <- getDefinition word env
        restTranslated <-  translateRegex rss env
        return $ '(' : translated ++ ')' : restTranslated
    else do
        restTranslated <- translateRegex rs env
        return $ '{' : restTranslated

        where
            search [] = ([],[])
            search (ssx:sxx)
                | ssx == '}' = ([],sxx)
                | otherwise =
                    let (word ,rss) = search sxx
                    in (ssx: word, rss)

translateRegex (r:rs) env = do
    restTranslated <- translateRegex rs env
    return $ r : restTranslated

-- If a ) is found without an opened one -> Error
-- if a ] is found without an opened one -> is okay :D 
tokeniseChar :: Parser String TokenRegex
tokeniseChar = Parser charP
    where
        charP [] = Left "EOS"
        charP ('\\' : x : xs) = Right (TChar x, xs)  -- Need to hangle \digit \xdigit
        charP ('(' : xs) = do 
            (tokenList, rest) <- inParenthesis xs
            Right (TGroup tokenList, rest)
        charP ('"' : xs) = do  -- Need to convert that in a list of char (can be \ (same as above))
            (content, rest) <- inQuote xs
            Right (TQuoting content, rest)
        charP ('|' : xs) = Right (TOr, xs)
        charP (')' : _) = Left "Parenthesis not opened"
        charP ('*' : xs) = Right (TRepetionMany, xs)
        charP ('+' : xs) = Right (TRepetionSome, xs)
        charP ('?' : xs) = Right (TRepetionMaybe, xs)
        charP ('{' : xs) = case trace (show $ span (/= '}') xs) (span (/= '}')) xs of 
                ([], _) -> trace "empty" Left "Empty {}"
                (inside, '}' : rest) -> trace "found" inCurly (span (/= ',') inside) rest
                _ -> trace "closed" Left "Not closed {}"
            -- inCurly break (/= '}') xs 
            -- case break (/= '}') xs of
            --     ([], _) -> Left "Patern {} empty"
            --     (input, rest) | all isDigit input -> Right (TRepetionCustom (read input) (read input), rest)
            --     (input, ',' : '}' :rest) | all isDigit input -> Right (TRepetionCustom (read input :: Int) (-1), rest )
            --     (input, rest)  -> case break (== ',') input of
            --         (a, ',' : b)
            --             | not (null a)
            --             , not (null b)
            --             , all isDigit a
            --             , all isDigit b
            --             -> Right (TRepetionCustom (read a) (read b), rest)
            --         _ -> Left "Invalid format" 
            
            
        charP ( x  : xs) = Right (TChar x, xs)

        inQuote ('"' :  xs) = Right ([], xs)
        inQuote ('\\': x : xs) = do
            (xr, xrs) <- inQuote xs
            Right (x : xr, xrs)
        inQuote (x : xs) = do
            (xr, xrs) <- inQuote xs
            Right (x : xr, xrs)
        inQuote [] = Left "Quote not closed"

        inParenthesis [] = Left "Parenthesis not closed"
        inParenthesis (')' : xs) = Right ([], xs)
        inParenthesis s = do
            (token, rest1) <- runParser tokeniseChar s
            (tokenRest, rest2 ) <- inParenthesis rest1
            Right (token: tokenRest, rest2)

        inCurly (n1, ',': n2)   rest | isDigits n1, isDigits n2 = Right (TRepetionCustom (read n1) (read n2), rest) 
        inCurly (n1, [','])     rest | isDigits n1              = Right (TRepetionCustom (read n1) (-1), rest)
        inCurly (n1, [])        rest | isDigits n1              = Right (TRepetionCustom (read n1) (read n1), rest)
        inCurly a               b                               = trace (('a' : show a )++ ' ' : 'b' : b)  Left "Ill formed {}"

        isDigits s = not (null s) && all isDigit s

regexParse :: Parser String [TokenRegex]
regexParse = many tokeniseChar