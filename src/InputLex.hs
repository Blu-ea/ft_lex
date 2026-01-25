{-# LANGUAGE InstanceSigs #-}
module InputLex where
import Control.Applicative ( Alternative(empty, (<|>), some, many) )

newtype Parser a = Parser{runParser :: InputString -> Maybe (a, InputString)}


instance Functor Parser where
    --fmap :: (a -> b) -> f a -> f b
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser x)=

        Parser $ \s -> do
            (x', s') <- x s
            return (f x', s')

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)

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
    fail _ = Parser $ const Nothing

instance Alternative Parser where
    empty = fail ""

    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just res -> Just res
            Nothing -> y s


--- -----------

data InputChar = InputChar {
    value :: Char,
    position :: (Int, FilePath)
} deriving Show


type InputString = [InputChar]


-- Atoms
type Regex     = String
type Action    = String
type CodeBlock = String
type MacroName = String

-- Whole file
data LexFile = LexFile
    { definitions :: [Definition]
    , rules       :: [Rule]
    } deriving Show

data Definition
    = Macro MacroName Regex
    | DCode CodeBlock
    | Array
    | Pointer
    deriving Show

data Rule
    = RRule Regex Action
    | RCode CodeBlock
    deriving Show

data LexItem
    = Def Definition
    | RuleItem Rule
    | UserCode String
    deriving Show

getInput :: [FilePath] -> IO InputString
getInput [] = return []
getInput paths = concat <$> mapM readFileWithLines paths
    where
        readFileWithLines path =
            annotate 1 path <$> readFile path

annotate :: Int -> FilePath -> String -> InputString
annotate _ _ [] = []
annotate n f (c:cs) =
    InputChar c (n, f) : annotate (if c == '\n' then n + 1 else n) f cs

-- linesS :: InputString -> [InputString]
-- linesS s = let (pre, suf) = break (\(InputChar c _) -> c == '\n') s
--     in pre : case suf of
--         [] -> []
--         (_:xs) -> linesS xs

-- recover :: Parser a -> Parser (Either String a)
-- recover p = Parser $ \s ->
--     case runParser p s of
--         Just (res, rest) -> Just (Right res, rest)
--         Nothing -> Just (Left "asd", dropWhile (not . (\(InputChar c _) -> c == '\n')) s)

consumeChar :: Char -> Parser Char
consumeChar c = Parser charP
    where
        charP [] = Nothing
        charP ((InputChar x _): xs) | x == c    = Just (c,xs)
                                    | otherwise = Nothing

consumeCharFromString :: String -> Parser Char
consumeCharFromString s = Parser charP
    where
        charP [] = Nothing
        charP ((InputChar x _): xs) | x `elem` s = Just (x, xs)
                                    | otherwise = Nothing

consumeAnyChar :: Parser Char
consumeAnyChar = Parser $ \s -> case s of 
        [] -> Nothing
        (InputChar x _ : xs) -> Just (x, xs)

consumeNotChar :: Char -> Parser Char
consumeNotChar c = Parser charP
    where
        charP [] = Nothing
        charP ((InputChar x _): xs) | x /= c    = Just (x, xs)
                                    | otherwise = Nothing

consumeString :: String -> Parser String
consumeString = mapM consumeChar

consumeUntil :: Char -> Parser String
consumeUntil = some <$> consumeNotChar

consumeUntilS :: String -> Parser String
consumeUntilS end = go
    where
        go = (lookAhead (consumeString end) *> pure[]) <|> ((:) <$> consumeAnyChar <*> go)


lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \s -> do
    (a, _) <- p s
    Just (a, s)

sectionSeparator :: Parser String
sectionSeparator = consumeString "%%"

space :: Parser Char
space = consumeCharFromString " \t"

ss :: Parser [Char]
ss = many space 

eol :: Parser ()
eol = 
    () <$ many space <* consumeChar '\n'

blankLines :: Parser ()
blankLines =
    () <$ many eol


defParse :: Parser Definition
defParse = 
    blankLines *> (
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
            <$> consumeUntil ' '
            <*  ss
            <*> consumeUntil '\n' <* eol
    )

ruleParse :: Parser Rule
ruleParse = Parser $ const Nothing


lexParse :: Parser LexFile
lexParse =
    LexFile
        <$> many defParse
        <*  sectionSeparator
        <*> some ruleParse
        <*  sectionSeparator
