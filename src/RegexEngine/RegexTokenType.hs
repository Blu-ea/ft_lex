module RegexEngine.RegexTokenType where 

type IsMatch = Bool


data BracketToken
    = BChar Char
    | BRange Char Char  -- [Char-Char]
    | BClass String     -- [:ClassName:]
    | BEquiv Char       -- [=Char=]
    | BCollating String -- [.String.]
    deriving (Show, Eq)

data TokenRegex 
    = TChar Char
    | TAny -- . -> Cannot be Null or Newline (0, 10)
    | TOr
    | TBracket IsMatch [BracketToken]
    | TQuoting String
    | TGroup [TokenRegex]
    | TRepetionMany  -- * -> (0, inf) (Take as much as possible)
    | TRepetionSome  -- + -> (1, inf) (Take as Much as possible)
    | TRepetionMaybe -- ? -> (0, 1) (Take as much as Possible)
    | TRepetionCustom Int Int -- {n,m} (Take between n and m)
    | TStart    -- ^ define the start of the String
    | TEnd      -- $ define the end of the String
    deriving (Show, Eq)



    -- Syntax Tree Regex
data SyntaxTreeRegex 
    = STConcat SyntaxTreeRegex SyntaxTreeRegex
    | STExpr Char
    | STAny
    | STOr SyntaxTreeRegex SyntaxTreeRegex
    | STBracket IsMatch [Char]
    | STQuote String
    | STGroup SyntaxTreeRegex
    | STRepetionMany SyntaxTreeRegex
    | STRepetionSome SyntaxTreeRegex
    | STRepetionMaybe SyntaxTreeRegex
    | STRepetionCustom Int Int SyntaxTreeRegex
    | STStart
    | STEnd
    |   STVoid -- Used to void Value, should not be in the return result
    deriving Show
