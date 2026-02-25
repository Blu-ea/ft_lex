module RegexEngine.RegexTokenType where 


data TokenRegex 
    = TChar Char
    | TOr
    | TBracket String
    | TQuoting String
    | TGroup [TokenRegex]
    | TRepetionMany  -- * -> (0, inf) (Take as much as possible)
    | TRepetionSome  -- + -> (1, inf) (Take as Much as possible)
    | TRepetionMaybe -- ? -> (0, 1) (Take as much as Possible)
    | TRepetionCustom Int Int -- {n,m} (Take between n and m)
    deriving Show

