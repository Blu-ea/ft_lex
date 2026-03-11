module RegexEngine.StringToRegex where

import InputDef.LexDefinition ( Definition, getDefinition)
import Parser
import RegexEngine.RegexTokenType

import GHC.Unicode
import Control.Applicative (Alternative(many))
import Debug.Trace
import Numeric (readHex)
import Data.Char


-- The regex need to be translated before hand so it can be processe token wise.
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

translateRegex ('\\': r: rs) env = do
    restTranslated <- translateRegex rs env
    return $ r : restTranslated
translateRegex ('"': rs) env =
    let (insideQuote, afterQuote) = inQuote rs in do
    restTranslated <- translateRegex afterQuote env
    return $ '"' : insideQuote ++ '"' : restTranslated
    where
        inQuote [] = ([], [])
        inQuote ('"' : iQrs) = ([], iQrs)
        inQuote (c : iQrs) = 
            let (inside, after) = inQuote iQrs in 
                (c : inside, after)

translateRegex ['['] _ = Nothing
translateRegex ('[':trs) env = do
    (inside, after) <- parseBracket trs
    rest <- translateRegex after env
    return $ '[' : inside ++ ']' : rest

translateRegex (r:rs) env = (r :) <$> translateRegex rs env 

takeWhileN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
takeWhileN 0 _ xs = ([], xs)
takeWhileN n f s@(c: xs)
        | n > 0 && f c= let (out, rest) = takeWhileN (n - 1) f xs
            in (c:out, rest)
        | otherwise = ([], s)
takeWhileN _ _ [] = ([], [])

parseBracket :: String -> Maybe (String, String)
parseBracket [] = Nothing
parseBracket s =
    let (start, rest) = case s of
                ('^':xs) -> ("^", xs)
                xs       -> ("", xs)
    in do
        (body, after) <- scanFirst rest
        return (start ++ body, after)
    where
        scanFirst (']':xs) = scan xs "]"
        scanFirst xs       = scan xs ""

        scan [] _ = Nothing
        scan (']':xs) acc = Just (acc, xs)

        scan ('[':'.':xs) acc = do
            (content, rest) <- takeUntil ".]" xs
            scan rest (acc ++ "[." ++ content ++ ".]")
        scan ('[':'=':xs) acc = do
            (content, rest) <- takeUntil "=]" xs
            scan rest (acc ++ "[=" ++ content ++ "=]")
        scan ('[':':':xs) acc = do
            (content, rest) <- takeUntil ":]" xs
            scan rest (acc ++ "[:" ++ content ++ ":]")

        scan ('\\': c :xs) acc = scan xs (acc ++ ['\\', c])
        scan (c:xs) acc = scan xs (acc ++ [c])

takeUntil :: String -> String -> Maybe (String, String)
takeUntil _ [] = Nothing
takeUntil pat s
    | pat `prefix` s = Just ("", drop (length pat) s)
    | otherwise = do
        (inside, rest) <- takeUntil pat (tail s)
        return (head s : inside, rest)

prefix :: String -> String -> Bool
prefix p s = take (length p) s == p

-- If a ) is found without an opened one -> Error
-- if a ] is found without an opened one -> is okay :D 
tokeniseChar :: Parser String TokenRegex
tokeniseChar = Parser charP
    where
        charP [] = Left "Empty Regex"
        charP ('.' : xs) = Right (TAny, xs)
        charP ('\\' : '\\' : xs) = Right (TChar '\\', xs)
        charP ('\\' : 'a' : xs) = Right (TChar '\a', xs)
        charP ('\\' : 'b' : xs) = Right (TChar '\b', xs)
        charP ('\\' : 'f' : xs) = Right (TChar '\f', xs)
        charP ('\\' : 'n' : xs) = Right (TChar '\n', xs)
        charP ('\\' : 'r' : xs) = Right (TChar '\r', xs)
        charP ('\\' : 't' : xs) = Right (TChar '\t', xs)
        charP ('\\' : 'v' : xs) = Right (TChar '\v', xs)
        charP ('\\' : 'x' : xs) = 
            let (nbstr, rest) = takeWhileN 2 isHexDigit xs
                nb = if null nbstr 
                    then 0 
                    else fst . head $ readHex nbstr
                in Right (TChar (chr nb), rest)
        charP ('\\' : n1: n2: n3: xs) | isOctDigit n1 && isOctDigit n2 && isOctDigit n3 = Right (TChar (chr (digitToInt n1 * (8 * 8) +digitToInt n2 * 8  + digitToInt n2)), xs)
        charP ('\\' : n1: n2: xs) | isOctDigit n1 && isOctDigit n2 = Right (TChar (chr (digitToInt n1 * 8 + digitToInt n2)), xs)
        charP ('\\' : n1: xs) | isOctDigit n1 = Right (TChar (chr . digitToInt $ n1), xs)
        charP ('\\' : x : xs) = Right (TChar x, xs)
        charP ('(' : xs) = do
            (tokenList, rest) <- inParenthesis xs
            Right (TGroup tokenList, rest)
        charP (')' : _) = Left "Parenthesis not opened"
        
        charP ('"' : xs) = do  -- Need to convert that in a list of char (can be \ (same as above))
            (content, rest) <- inQuote xs
            Right (TQuoting content, rest)

        charP ('[' : rs) = Left ""

        charP ('*' : xs) = Right (TRepetionMany, xs)
        charP ('+' : xs) = Right (TRepetionSome, xs)
        charP ('?' : xs) = Right (TRepetionMaybe, xs)
        charP ('{' : xs) = case trace (show $ span (/= '}') xs) (span (/= '}')) xs of
                ([], _) -> trace "empty" Left "Empty {}"
                (inside, '}' : rest) -> trace "found" inCurly (span (/= ',') inside) rest
                _ -> trace "closed" Left "Not closed {}"

        charP ('|' : xs) = Right (TOr, xs)

        charP ( x  : xs) = Right (TChar x, xs)

        -- ============ --
        -- End of CharP --
        -- ============ --

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