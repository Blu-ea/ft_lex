module RegexEngine.StringToRegex where

import Control.Applicative (Alternative(some), optional)
import Numeric (readHex)
import Data.Char
    ( digitToInt,
      chr,
      isAlpha,
      isAlphaNum,
      isDigit,
      isHexDigit,
      isOctDigit )
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import InputDef.LexDefinition ( Definition, getDefinition)
import Parser ( Parser(..) )
import RegexEngine.RegexTokenType
    ( BracketToken(BChar, BCollating, BClass, BEquiv, BRange), TokenRegex(..) )


-- The regex need to be translated before hand so it can be processe token wise.
translateRegex :: String -> (Int, FilePath) -> [Definition] -> Either String (String, (Int,FilePath))
translateRegex [] pos _ = Right ([], pos)
translateRegex ('{':rs) pos@(x, file) env =
    let (word, rss) = search rs in  

    if not (null word) && isAlpha (head word) && all isAlphaNum (tail word) then do
        let translation = getDefinition word env
        (restTranslated, _) <-  translateRegex rss pos env
        case translation of 
            Nothing -> Left (file ++ ':' : show x ++ ": Definition {" ++ word ++ "} is undefined.")
            Just translated -> return ('(' : translated ++ ')' : restTranslated, pos)
    else do
        (restTranslated, _) <- translateRegex rs pos env
        return ('{' : restTranslated, pos)

        where
            search [] = ([],[])
            search (ssx:sxx)
                | ssx == '}' = ([],sxx)
                | otherwise =
                    let (word ,rss) = search sxx
                    in (ssx: word, rss)

translateRegex ('\\': r: rs) pos env = do
    (restTranslated, _) <- translateRegex rs pos env
    return (r : restTranslated, pos)
translateRegex ('"': rs) pos env =
    let (insideQuote, afterQuote) = inQuote rs in do
    (restTranslated, _) <- translateRegex afterQuote pos env
    return ('"' : insideQuote ++ '"' : restTranslated, pos)
    where
        inQuote [] = ([], [])
        inQuote ('"' : iQrs) = ([], iQrs)
        inQuote (c : iQrs) = 
            let (inside, after) = inQuote iQrs in 
                (c : inside, after)

-- translateRegex ['['] _ _ = trace " Here ? 1" Nothing
translateRegex ('[':trs) pos env = do
    (inside, after) <- parseBracket trs
    (rest, _)<- translateRegex after pos env
    return ('[' : inside ++ ']' : rest, pos)

translateRegex (r:rs) pos env = do
    (rest, _) <- translateRegex rs pos env 
    return (r : rest, pos)

takeWhileN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
takeWhileN 0 _ xs = ([], xs)
takeWhileN n f s@(c: xs)
        | n > 0 && f c= let (out, rest) = takeWhileN (n - 1) f xs
            in (c:out, rest)
        | otherwise = ([], s)
takeWhileN _ _ [] = ([], [])

parseBracket :: String -> Either String (String, String)
parseBracket [] = Left "Bracket Expression not closed"
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

        scan [] _ = Left "Bracket Expression not Closed"
        scan (']':xs) acc = Right (acc, xs)

        scan ('[':'.':xs) acc =
            case takeUntil ".]" xs of 
                Nothing -> Left "Bracket Expression [. not closed"
                Just (content, rest) -> scan rest (acc ++ "[." ++ content ++ ".]")
        scan ('[':'=':xs) acc =
            case takeUntil "=]" xs of 
                Nothing -> Left "Bracket Expression [= not closed"
                Just (content, rest) -> scan rest (acc ++ "[=" ++ content ++ "=]")

        scan ('[':':':xs) acc =
            case takeUntil ":]" xs of 
                Nothing -> Left "Bracket Expression [: not closed"
                Just (content, rest) -> scan rest (acc ++ "[:" ++ content ++ ":]")


        scan ('\\': c :xs) acc = scan xs (acc ++ ['\\', c])
        scan (c:xs) acc = scan xs (acc ++ [c])

takeUntil :: String -> String -> Maybe (String, String)
takeUntil _ [] = Nothing
takeUntil pat s
    | pat `isPrefixOf` s = Just ("", drop (length pat) s)
    | otherwise = do
        (inside, rest) <- takeUntil pat (tail s)
        return (head s : inside, rest)

-- If a ) is found without an opened one -> Error
-- if a ] is found without an opened one -> is okay :D 
tokeniseChar :: Parser String TokenRegex
tokeniseChar = Parser charP
    where
        charP [] = Left "Empty Regex"

        charP ('.' : xs) = Right (TAny, xs)
        charP ['$'] = Right (TEnd, [])

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

        charP ('[' : rs) = do
            (inside, outside) <- parseBracket rs
            let inverted = "^" `isPrefixOf` inside
            (content, rest) <- tokeniseBracketExpreFirst (if inverted then tail inside else inside) :: Either String ([BracketToken], String)
            if not $ null rest
                then Left "Ill Formed Bracket Expression"
                else Right (TBracket inverted content , outside) 
        charP ('*' : xs) = Right (TRepetionMany, xs)
        charP ('+' : xs) = Right (TRepetionSome, xs)
        charP ('?' : xs) = Right (TRepetionMaybe, xs)
        charP ('{' : xs) = case span (/= '}') xs of
                ([], _) -> Left "Empty {}"
                (inside, '}' : rest) -> inCurly (span (/= ',') inside) rest
                _ -> Left "Not closed {}"

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
        inCurly _               _                               = Left "Ill formed {}"

        isDigits s = not (null s) && all isDigit s

        tokeniseBracketExpreFirst (']' :rs) = do
            (otherToken, rest) <- tokeniseBracketExpre rs
            Right (BChar ']' : otherToken, rest)
        tokeniseBracketExpreFirst s = tokeniseBracketExpre s

        tokeniseBracketExpre [] = Right ([], "")
        tokeniseBracketExpre (c1:'-':c2:rs) =
            if c1 > c2 then Left "Error on Range"
            else do
                (otherToken, rest) <- tokeniseBracketExpre rs
                Right (BRange c1 c2 : otherToken, rest)
        tokeniseBracketExpre ('[':'.':rs) = do
            (content, after) <- maybeToEither "Error `.]`" (takeUntil ".]" rs)
            (otherToken, rest) <- tokeniseBracketExpre after
            Right (BCollating content : otherToken, rest)
        tokeniseBracketExpre ('[':':':rs) = do
            (content, after) <- maybeToEither "Error `:]`" (takeUntil ":]" rs)
            (otherToken, rest) <- tokeniseBracketExpre after
            Right (BClass content : otherToken, rest)
        tokeniseBracketExpre ('[':'=':rs) = do
            (content, after) <- maybeToEither "Error `=]`" (takeUntil "=]" rs)
            (otherToken, rest) <- tokeniseBracketExpre after
            if length content == 1 
                then Right (BEquiv (head content) : otherToken, rest)
                else Left "Content is to long inside [= =]"

        tokeniseBracketExpre ('\\' : c : rs) = do
            (otherToken, rest) <- tokeniseBracketExpre rs
            Right (BChar c : otherToken, rest)
        tokeniseBracketExpre (c : rs) = do
            (otherToken, rest) <- tokeniseBracketExpre rs
            Right (BChar c : otherToken, rest)

startAnchor :: Parser String [TokenRegex]
startAnchor = Parser (\ 
    input -> case input of
        ('^' : rs)-> Right([TStart], rs)
        _ -> Left "" )


regexParse :: Parser String [TokenRegex]
regexParse = (++)
                    <$> (fromMaybe [] <$> optional startAnchor)
                    <*> some tokeniseChar

maybeToEither :: e -> Maybe a -> Either e a 
maybeToEither err = maybe (Left err) Right
