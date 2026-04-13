{-# LANGUAGE LambdaCase #-}
module RegexEngine.RegexParser where

import RegexEngine.RegexTokenType (TokenRegex (..), SyntaxTreeRegex (..), BracketToken (..))
import Parser ( Parser (Parser, runParser) )
import Control.Applicative ((<|>))
import Control.Monad (void)


parserAtom :: Parser [TokenRegex] SyntaxTreeRegex
parserAtom = Parser tokenize
    where
        tokenize ((TChar c):x) = Right (STExpr c, x)
        tokenize (TAny:x) = Right (STAny, x)
        tokenize ((TQuoting s):x) = Right (STQuote s, x)
        tokenize ((TBracket b brTokens):x) = do
            content <- mapM temp . filter (not . isCollating) $ brTokens
            collating <- mapM temp . filter isCollating $ brTokens -- for the moment, will be merge later on with content
            Right (
                STBracket b (concat content) collating ,x)
                where
                    isCollating (BCollating _ ) = True
                    isCollating _ = False
        tokenize ((TGroup content):x) = do
            treeContent <- runParser parserOr content
            case treeContent of
                (tC, []) -> Right (STGroup tC, x)
                (_, _) -> Left "Unvalid Expresion"
        tokenize (TStart:x) = Right (STStart, x)
        tokenize (TEnd:x) = Right (STEnd, x)

        tokenize _ = Left ""

temp :: BracketToken -> Either String String
temp (BChar c) = Right [c]
temp (BRange c1 c2) = Right [c1..c2]
temp (BClass s) = case s of
    "alnum" -> Right (['a'..'z'] ++ ['A'..'Z'] ++['0'..'9'])
    "lower" -> Right ['a'..'z']
    "upper" -> Right ['A'..'Z']
    "alpha" -> Right (['a'..'z'] ++ ['A'..'Z'])
    "digit" -> Right ['0'..'9']
    "xdigit" -> Right (['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9'])
    "cntrl" -> Right (['\x00'..'\x1F'] ++ ['\x7F'])
    "space" -> Right [' ', '\f', '\n', '\r', '\t', '\v']
    "blank" -> Right [' ','\t']
    "print" -> Right ['\x20'..'\x7E']
    "graph" -> Right ['\x21'..'\x7E']
    "punct" -> Right ['!','"','#','$','%','&','\'','(',')','*','+',',','\\','-','.','/',':',';','<','=','>','?','@','[',']','^','_','`','{','|','}','~']
    unkownClass -> Left $ "Collating Class not found :" ++ unkownClass
temp (BEquiv c) = Right [c]
temp (BCollating s) = Right "TODO by Posix def"


parseRepetition :: Parser[TokenRegex] SyntaxTreeRegex
parseRepetition = do
    base <- parserAtom
    Parser (\case
        TRepetionMany: rs -> pure (STRepetionMany base, rs)
        TRepetionSome: rs -> pure (STRepetionSome base, rs)
        TRepetionMaybe: rs -> pure (STRepetionMaybe base, rs)
        _ -> Left "")
        <|> pure base

parserConcat :: Parser [TokenRegex] SyntaxTreeRegex
parserConcat = do
    first <- parseRepetition
    rest first
    where
        rest left =
            (do
                right <- parserConcat
                rest (STConcat left right)
            ) <|> pure left

parseInterval :: Parser[TokenRegex] SyntaxTreeRegex
parseInterval = do
    base <- parserConcat
    Parser (\case
        [TRepetionCustom n1 n2]    -> do
            pure  (STRepetionCustom n1 n2 base, [])
        (TRepetionCustom n1 n2:rs) -> do
            (other, after) <- runParser parseInterval rs
            pure (STConcat (STRepetionCustom n1 n2 base) other, after)
        _ -> Left "")
        <|> pure base

parserOr :: Parser [TokenRegex] SyntaxTreeRegex
parserOr = do
    first <- parseInterval
    rest first
    where
        rest left =
            (do
                void consumeOr
                right <- parseInterval
                rest (STOr left right)
            ) <|> pure left
            where
            consumeOr = Parser $ \case
                (TOr: rs)   -> Right (STVoid, rs)
                _           -> Left ""


regexParse :: [TokenRegex] -> Either String SyntaxTreeRegex
regexParse input = do
    (tree, rest) <- runParser parserOr input
    if null rest then Left "Invalid Rule."
    else Right tree