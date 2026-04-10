{-# LANGUAGE LambdaCase #-}
module RegexEngine.RegexParser where

import RegexEngine.RegexTokenType (TokenRegex (..), SyntaxTreeRegex (..))
import Parser ( Parser (Parser, runParser) )
import Control.Applicative ((<|>))
import Control.Monad (void)


parserAtom :: Parser [TokenRegex] SyntaxTreeRegex
parserAtom = Parser tokenize
    where
        tokenize ((TChar c):x) = Right (STExpr c, x)
        tokenize (TAny:x) = Right (STAny, x)
        tokenize ((TQuoting s):x) = Right ((STQuote s), x)
        tokenize ((TBracket b brTokens):x) = Right ((STBracket b "TODO"), x) -- TODO: bracket Expression
        tokenize ((TGroup content):x) = Right ((STGroup STAny), x) -- TODO
        tokenize (TStart:x) = Right (STStart, x)
        tokenize (TEnd:x) = Right (STEnd, x)

        tokenize _ = Left ""


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