{-# LANGUAGE LambdaCase #-}
module ParserDef.InputLex where

import Control.Applicative

import ParserDef.InputChar ( InputString, InputChar(InputChar) )
import ParserDef.Parser ( Parser (Parser) )
import ParserDef.LexDefinition
    ( Rule(..), Definition(..), LexFile(LexFile) )

import Debug.Trace
import Data.Functor (($>), void)


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

-- Parser a
--  is equivalent to
-- InputChar -> Either String (a, InputString)

consumeChar :: Char -> Parser Char
consumeChar c = Parser ( trace ("Consume"++[c]) charP)
    where
        charP [] = Left $ "Expected `" ++ [c] ++ "`, but found end of file"
        charP ((InputChar x (line, file)): xs) | x == c    = Right (c,xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ show line ++ ": Unexpected char `"++ show x ++"`") 

consumeCharFromString :: String -> Parser Char
consumeCharFromString s = Parser charP
    where
        charP [] = Left $ "Expected any from`" ++ s ++ "`, but found end of file"
        charP ((InputChar x (line, file)): xs) | x `elem` s = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ show line ++ ": Unexpected char `"++ show x ++"`")

consumeAnyChar :: Parser Char
consumeAnyChar = Parser $ \case
        [] -> Left "End of file"
        (InputChar x _ : xs) -> Right (x, xs)

consumeNotChar :: Char -> Parser Char
consumeNotChar c = Parser charP
    where
        charP [] = Left "End of file"
        charP ((InputChar x (line, file)): xs) | x /= c    = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ show line ++ ": Unexpected char `"++ show x ++"`")

consumeNotCharFromString :: String -> Parser Char
consumeNotCharFromString s = Parser charP
    where
        charP [] = Left "End of File"
        charP ((InputChar x (line, file)): xs) | x `notElem` s = Right (x, xs)
                                    | otherwise = Left ("Error: " ++ file ++ ":" ++ show line ++ ": Unexpected char `"++ show x ++"`")


consumeString :: String -> Parser String
consumeString = mapM consumeChar

consumeUntil :: Char -> Parser String
consumeUntil c = some (consumeNotChar c)

consumeUntilS :: String -> Parser String
consumeUntilS end = go
    where
        go = (lookAhead (consumeString end) $> []) <|> ((:) <$> consumeAnyChar <*> go)

consumeUntilEither :: String -> Parser String
consumeUntilEither endC = go
    where
        go = lookAhead (consumeCharFromString endC $> [])<|> ((:) <$> consumeAnyChar <*> go)

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \s -> do
    (a, _) <- p s
    Right (a, s)

checkIfNotCharThenConsume :: [Char] -> Parser a -> Parser a
checkIfNotCharThenConsume s (Parser f) = Parser charP
    where
        charP [] = Left "End of file"
        charP xss@((InputChar x _): _)  | x `notElem` s = f xss
                                        | otherwise = Left "Found ill formed data"

sectionSeparator :: Parser String
sectionSeparator = consumeString "%%" <|> Parser (\_ -> Left "Error: Expected section separator")

space :: Parser Char
space = consumeCharFromString " \t"

ss :: Parser [Char]
ss = many space 

eol :: Parser ()
eol = 
    void $ many space <* consumeChar '\n'

blankLines :: Parser ()
blankLines = 
    void $ many eol


consumeBrackets :: Parser String
consumeBrackets = consumeChar '{' *> (concat <$> many consumeInner) <* consumeChar '}'
    where consumeInner = 
            consumeBrackets <|> many (consumeNotCharFromString "{}")

consumeDQuotes :: Parser String
consumeDQuotes = consumeChar '"' *> consumeUntil '"' <* consumeChar '"'


defParse :: Parser [Definition]
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
            <$> consumeUntilEither " \t\n" -- Does not consume the white space, if a \n is found -> error
            <*  ss
            <*> checkIfNotCharThenConsume "\n" (consumeUntil '\n') <* eol
    )

ruleParse :: Parser [Rule]
ruleParse = 
    (++)
        <$> many (blankLines *> rCode) 
        <*> some (blankLines *> rRule)
    where
        rCode = RCode <$> (space *> (ss *> consumeUntil '\n' <* eol))
            <|> RCode <$> (consumeString "%{\n" *> consumeUntilS "\n%}\n" <* consumeString "\n%}\n")
        rRule = RRule <$> (checkIfNotCharThenConsume " %" (consumeUntil ' ') <* ss) 
                        <*> ( consumeBrackets <|> consumeUntil '\n') <* eol


lexParse :: Parser LexFile
lexParse =
    LexFile
        <$> defParse
        <*  (blankLines *> sectionSeparator)
        <*> trace "\n\nMaybe here 2 ?" ruleParse
        <*  trace "\n\nMaybe here %% 3 ?" (blankLines *> traceShowId sectionSeparator)
        <*> trace "\n\nMaybe here Anything left4 ?" many (traceShowId consumeAnyChar)
