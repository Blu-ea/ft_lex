module RegexEngine.RegexTokenType where 

data BracketToken
    = BChar Char
    | BRange Char Char  -- [Char-Char]
    | BClass String     -- [:ClassName:]
    | BEquiv Char       -- [=Char=]
    | BCollating String -- [.String.]
    deriving Show


data TokenRegex 
    = TChar Char
    | TAny -- . -> Cannot be Null or Newline (0, 10)
    | TOr
    | TBracket Bool [BracketToken]
    | TQuoting String
    | TGroup [TokenRegex]
    | TRepetionMany  -- * -> (0, inf) (Take as much as possible)
    | TRepetionSome  -- + -> (1, inf) (Take as Much as possible)
    | TRepetionMaybe -- ? -> (0, 1) (Take as much as Possible)
    | TRepetionCustom Int Int -- {n,m} (Take between n and m)
    | TStart    -- ^ define the start of the String
    | TEnd      -- $ define the end of the String
    deriving Show

