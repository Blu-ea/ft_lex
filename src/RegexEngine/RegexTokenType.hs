module RegexEngine.RegexTokenType where
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)

type IsMatch = Bool


data BracketToken
    = BChar Char
    | BRange Char Char  -- [Char-Char]
    | BClass String     -- [:ClassName:]
    | BEquiv Char       -- [=Char=]
    | BCollating String -- [.String.]
    deriving (Show, Eq)

brTkToStr :: BracketToken -> Either String String
brTkToStr (BChar c) = Right [c]
brTkToStr (BRange c1 c2) = Right [c1..c2]
brTkToStr (BClass s) = case s of
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
brTkToStr (BEquiv c) = Right [c]
brTkToStr (BCollating s) = let c = collattionOrder s in 
    if isJust c then Right [fromJust c] else Left $ "Collating Order not found :" ++ s



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

instance Show SyntaxTreeRegex where
    show (STConcat t1 t2) = show t1 ++ show t2
    show (STExpr c) = [c]
    show STAny = "."
    show (STOr t1 t2) = show t1 ++ '|' : show t2
    show (STBracket isMatch content) = '[' : (if isMatch then "" else "^") ++ content ++ "]"
    show (STQuote content) = show content
    show (STGroup t) = '(' : show t ++ ")"
    show (STRepetionMany t) = show t ++ "*"
    show (STRepetionSome t) = show t ++ "+"
    show (STRepetionMaybe t) = show t ++ "?"
    show (STRepetionCustom n1 n2 t) = show t ++ '{' : show n1 ++ ',' : show n2 ++ "}"
    show STStart = "^"
    show STEnd = "$"
    show STVoid = ":VOID:"


collattionOrder :: String -> Maybe Char
collattionOrder "NUL" = Just '\0'
collattionOrder "SOH" = Just '\1'
collattionOrder "STX" = Just '\2'
collattionOrder "ETX" = Just '\3'
collattionOrder "EOT" = Just '\4'
collattionOrder "ENQ" = Just '\5'
collattionOrder "ACK" = Just '\6'
collattionOrder "alert" = Just '\7'
collattionOrder "backspace" = Just '\8'
collattionOrder "tab" = Just '\9'
collattionOrder "newline" = Just '\10'
collattionOrder "vertical-tab" = Just '\11'
collattionOrder "form-feed" = Just '\12'
collattionOrder "carriage-return" = Just '\13'
collattionOrder "SO" = Just '\14'
collattionOrder "SI" = Just '\15'
collattionOrder "DLE" = Just '\16'
collattionOrder "DC1" = Just '\17'
collattionOrder "DC2" = Just '\18'
collattionOrder "DC3" = Just '\19'
collattionOrder "DC4" = Just '\20'
collattionOrder "NAK" = Just '\21'
collattionOrder "SYN" = Just '\22'
collattionOrder "ETB" = Just '\23'
collattionOrder "CAN" = Just '\24'
collattionOrder "EM" = Just '\25'
collattionOrder "SUB" = Just '\26'
collattionOrder "ESC" = Just '\27'
collattionOrder "IS4" = Just '\28'
collattionOrder "IS3" = Just '\29'
collattionOrder "IS2" = Just '\30'
collattionOrder "IS1" = Just '\31'
collattionOrder "space" = Just '\32'
collattionOrder "exclamation-mark" = Just '\33'
collattionOrder "quotation-mark" = Just '\34'
collattionOrder "number-sign" = Just '\35'
collattionOrder "dollar-sign" = Just '\36'
collattionOrder "percent-sign" = Just '\37'
collattionOrder "ampersand" = Just '\38'
collattionOrder "apostrophe" = Just '\39'
collattionOrder "left-parenthesis" = Just '\40'
collattionOrder "right-parenthesis" = Just '\41'
collattionOrder "asterisk" = Just '\42'
collattionOrder "plus-sign" = Just '\43'
collattionOrder "comma" = Just '\44'
collattionOrder "hyphen-minus" = Just '\45'
collattionOrder "period" = Just '\46'
collattionOrder "slash" = Just '\47'
collattionOrder "zero" = Just '\48'
collattionOrder "one" = Just '\49'
collattionOrder "two" = Just '\50'
collattionOrder "three" = Just '\51'
collattionOrder "four" = Just '\52'
collattionOrder "five" = Just '\53'
collattionOrder "six" = Just '\54'
collattionOrder "seven" = Just '\55'
collattionOrder "eight" = Just '\56'
collattionOrder "nine" = Just '\57'
collattionOrder "colon" = Just '\58'
collattionOrder "semicolon" = Just '\59'
collattionOrder "less-than-sign" = Just '\60'
collattionOrder "equals-sign" = Just '\61'
collattionOrder "greater-than-sign" = Just '\62'
collattionOrder "question-mark" = Just '\63'
collattionOrder "commercial-at" = Just '\64'
collattionOrder "A" = Just '\65'
collattionOrder "B" = Just '\66'
collattionOrder "C" = Just '\67'
collattionOrder "D" = Just '\68'
collattionOrder "E" = Just '\69'
collattionOrder "F" = Just '\70'
collattionOrder "G" = Just '\71'
collattionOrder "H" = Just '\72'
collattionOrder "I" = Just '\73'
collattionOrder "J" = Just '\74'
collattionOrder "K" = Just '\75'
collattionOrder "L" = Just '\76'
collattionOrder "M" = Just '\77'
collattionOrder "N" = Just '\78'
collattionOrder "O" = Just '\79'
collattionOrder "P" = Just '\80'
collattionOrder "Q" = Just '\81'
collattionOrder "R" = Just '\82'
collattionOrder "S" = Just '\83'
collattionOrder "T" = Just '\84'
collattionOrder "U" = Just '\85'
collattionOrder "V" = Just '\86'
collattionOrder "W" = Just '\87'
collattionOrder "X" = Just '\88'
collattionOrder "Y" = Just '\89'
collattionOrder "Z" = Just '\90'
collattionOrder "left-square-bracket" = Just '\91'
collattionOrder "backslash" = Just '\92'
collattionOrder "right-square-bracket" = Just '\93'
collattionOrder "circumflex" = Just '\94'
collattionOrder "underscore" = Just '\95'
collattionOrder "grave-accent" = Just '\96'
collattionOrder "a" = Just '\97'
collattionOrder "b" = Just '\98'
collattionOrder "c" = Just '\99'
collattionOrder "d" = Just '\100'
collattionOrder "e" = Just '\101'
collattionOrder "f" = Just '\102'
collattionOrder "g" = Just '\103'
collattionOrder "h" = Just '\104'
collattionOrder "i" = Just '\105'
collattionOrder "j" = Just '\106'
collattionOrder "k" = Just '\107'
collattionOrder "l" = Just '\108'
collattionOrder "m" = Just '\109'
collattionOrder "n" = Just '\110'
collattionOrder "o" = Just '\111'
collattionOrder "p" = Just '\112'
collattionOrder "q" = Just '\113'
collattionOrder "r" = Just '\114'
collattionOrder "s" = Just '\115'
collattionOrder "t" = Just '\116'
collattionOrder "u" = Just '\117'
collattionOrder "v" = Just '\118'
collattionOrder "w" = Just '\119'
collattionOrder "x" = Just '\120'
collattionOrder "y" = Just '\121'
collattionOrder "z" = Just '\122'
collattionOrder "left-curly-bracket" = Just '\123'
collattionOrder "vertical-line" = Just '\124'
collattionOrder "right-curly-bracket" = Just '\125'
collattionOrder "tilde" = Just '\126'
collattionOrder "DEL" = Just '\127'
collattionOrder _ = Nothing