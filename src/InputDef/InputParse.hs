{-# LANGUAGE LambdaCase #-}
module InputDef.InputParse where

import Control.Applicative

import InputDef.InputChar ( InputString, InputChar(InputChar), getPose )
import InputDef.LexDefinition ( Rule(..), Definition(..), LexFile(LexFile) )
import Parser ( Parser (Parser, runParser) )

import Debug.Trace
import Data.Functor (($>), void)
import Data.Char ( isAlphaNum, isAlpha )


getInput :: [FilePath] -> IO InputString
getInput [] = return []
getInput paths = concat <$> mapM readFileWithLines paths
    where
        readFileWithLines path =
            annotate 1 1 path <$> readFile path

annotate :: Int -> Int -> FilePath -> String -> InputString
annotate _ _ _ [] = []
annotate ln cn f (c:cs) =
    InputChar c ((ln, cn), f) : annotate (if c == '\n' then ln + 1 else ln)
                                                        (if c == '\n' then 0 else cn + 1)
                                                        f cs

-- Parser i a
--  is equivalent to
-- i -> Either String (a, i)

consumeChar :: Char -> Parser [InputChar] Char
consumeChar c = Parser ( trace ("Consume"++[c]) charP)
    where
        charP [] = Left $ "Expected `" ++ [c] ++ "`, but found end of file"
        charP ((InputChar x (pose, file)): xs)  | x == c    = Right (c,xs)
                                                | otherwise = Left ("Error: " ++ file ++ ':' : getPose pose ++ ": Unexpected char `"++ show x ++"`") 

consumeCharFromString :: String -> Parser [InputChar] Char
consumeCharFromString s = Parser charP
    where
        charP [] = Left $ "Expected any from`" ++ s ++ "`, but found end of file"
        charP ((InputChar x (pose, file)): xs) | x `elem` s = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ getPose pose ++ ": Unexpected char `"++ show x ++"`")

consumeAnyChar :: Parser [InputChar] Char
consumeAnyChar = Parser $ \case
        [] -> Left "End of file"
        (InputChar x _ : xs) -> trace ("Consume " ++ show x )Right (x, xs)

consumeNotChar :: Char -> Parser [InputChar] Char
consumeNotChar c = Parser charP
    where
        charP [] = Left "End of file"
        charP ((InputChar x (pose, file)): xs) | x /= c    = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ getPose pose ++ ": Unexpected char `"++ show x ++"`")

consumeNotCharFromString :: String -> Parser [InputChar] Char
consumeNotCharFromString s = Parser charP
    where
        charP [] = Left "End of File"
        charP ((InputChar x (pose, file)): xs) | x `notElem` s = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ getPose pose ++ ": Unexpected char `"++ show x ++"`")


consumeString :: String -> Parser [InputChar] String
consumeString = mapM consumeChar

consumeUntil :: Char -> Parser [InputChar] String
consumeUntil c = some (consumeNotChar c)

consumeUntilS :: String -> Parser [InputChar] String
consumeUntilS end = go
    where
        go = (lookAhead (consumeString end) $> []) <|> ((:) <$> consumeAnyChar <*> go)

consumeUntilEither :: String -> Parser [InputChar] String
consumeUntilEither endC = trace ("Test Here ?? until" ++ endC) go
    where
        go = lookAhead (consumeCharFromString endC $> [])<|> ((:) <$> consumeAnyChar <*> go)

consumeUntilFalse :: (Char -> Bool) -> Parser [InputChar] String
consumeUntilFalse f = some $ checkIfTrueThenConsume f consumeAnyChar

lookAhead :: Parser i o -> Parser i o
lookAhead (Parser p) = Parser $ \s -> do
    (a, _) <- p s
    Right (a, s)

checkIfNotCharThenConsume :: [Char] -> Parser [InputChar] a -> Parser [InputChar] a
checkIfNotCharThenConsume s (Parser f) = Parser charP
    where
        charP [] = Left "End of file"
        charP xss@((InputChar x _): _)  | x `notElem` s = f xss
                                        | otherwise = Left "Found ill formated data"

checkIfTrueThenConsume :: (Char -> Bool) -> Parser [InputChar] a -> Parser [InputChar] a
checkIfTrueThenConsume f (Parser f') = Parser charP
    where
        charP [] = Left "End of file"
        charP xss@(InputChar x _: _)    | f x = f' xss
                                        | otherwise = Left "Found ill formated data"

sectionSeparator :: Parser  [InputChar] String
sectionSeparator = consumeString "%%" <|> Parser (\_ -> Left "Error: Expected section separator")

space :: Parser [InputChar] Char
space = consumeCharFromString " \t"

ss :: Parser [InputChar] [Char]
ss = many space 

eol :: Parser [InputChar] ()
eol = 
    void $ many space <* consumeChar '\n'

blankLines :: Parser [InputChar] ()
blankLines = 
    void $ many eol


consumeBrackets :: Parser [InputChar] String
consumeBrackets = do
    open <- consumeChar '{'
    inner <- concat <$> many innerBracket
    close <- consumeChar '}'
    return (open : inner ++ [close])
    where 
        innerBracket = (:[]) <$> checkIfNotCharThenConsume "{}" consumeAnyChar <|> consumeBrackets

consumeDQuotes :: Parser [InputChar] String
consumeDQuotes = do
    open <- consumeChar '"'
    inner <- consumeUntil '"'
    close <- consumeChar '"'
    return (open : inner ++ [close])


defParse :: Parser [InputChar] [Definition]
defParse = 
    many $ blankLines *> (
        Array
            <$ consumeString "%array" <* eol 
        <|>
        Pointer
            <$ consumeString "%pointer" <* eol
        <|>
        DCode
            <$> (space *> consumeUntil '\n' <* eol)
        <|>
        DCode
            <$> (consumeString "%{\n" *> consumeUntilS "\n%}\n" <* consumeString "\n%}\n")
        <|>
        Macro
            <$> checkIfTrueThenConsume isAlpha (consumeUntilFalse isAlphaNum) -- Does not consume the white space, if a \n is found -> error
            <*  space
            <*  ss
            <*> checkIfNotCharThenConsume "\n" (consumeUntil '\n') <* eol
    )

ruleParse :: Parser [InputChar] [Rule]
ruleParse = 
    (++)
        <$> many (blankLines *> rCode) 
        <*> some (blankLines *> rRule)
    where
        rCode = RCode <$> (space *> (ss *> consumeUntil '\n' <* eol))
            <|> RCode <$> (consumeString "%{\n" *> consumeUntilS "\n%}\n" <* consumeString "\n%}\n")
        rRule = RRule <$> (checkIfNotCharThenConsume " %" (concat <$> some (some (consumeNotCharFromString " \"") <|> consumeDQuotes)) <* ss)
                        <*> (concat <$> some (some (consumeNotCharFromString "\n{}") <|> consumeBrackets)) <* eol


lexParse :: Parser [InputChar] LexFile
lexParse =
    LexFile
        <$> defParse
        <*  (blankLines *> sectionSeparator)
        <*> trace "\n\n -- Start Rule Parse" ruleParse
        <*  trace "\n\n -- Search SectionSeparator" (blankLines *> traceShowId sectionSeparator)
        <*> trace "\n\n -- Start End point" many (traceShowId consumeAnyChar)
