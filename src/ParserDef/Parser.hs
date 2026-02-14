module ParserDef.Parser where

import ParserDef.InputChar (InputString)
import Control.Applicative ( Alternative(empty, (<|>)) )



newtype Parser a = Parser
    { runParser 
        :: InputString -> Either String (a, InputString) 
    }

instance Show (Parser a) where
    show _ = "<parser>"



instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser x)=

        Parser $ \s -> do
            (x', s') <- x s
            return (f x', s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)

-- (<*>) :: Parser (a->b) -> Parser a -> Parser B
    (Parser f) <*> (Parser x) =
        Parser $ \s -> do
            (f', s1) <- f s
            (x', s2) <- x s1
            return (f' x', s2)

instance Monad Parser where
    (Parser x) >>= f =
        Parser $ \s -> do
            (a', s') <- x s
            runParser (f a') s'

instance MonadFail Parser where
    fail s = Parser $ const $ Left s

instance Alternative Parser where
    empty = fail ""

    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Right res -> Right res
            Left err1 -> case y s of
                Right res -> Right res
                Left err2 -> Left $ unlines [err1, err2]
