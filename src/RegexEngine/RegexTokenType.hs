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
brTkToStr (BCollating s) = let c = colattionOrder s in 
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


colattionOrder :: String -> Maybe Char
colattionOrder "NUL" = Just '\0'
colattionOrder "SOH" = Just '\1'
colattionOrder "STX" = Just '\2'
colattionOrder "ETX" = Just '\3'
colattionOrder "EOT" = Just '\4'
colattionOrder "ENQ" = Just '\5'
colattionOrder "ACK" = Just '\6'
colattionOrder "alert" = Just '\7'
colattionOrder "backspace" = Just '\8'
colattionOrder "tab" = Just '\9'
colattionOrder "newline" = Just '\10'
colattionOrder "vertical-tab" = Just '\11'
colattionOrder "form-feed" = Just '\12'
colattionOrder "carriage-return" = Just '\13'
colattionOrder "SO" = Just '\14'
colattionOrder "SI" = Just '\15'
colattionOrder "DLE" = Just '\16'
colattionOrder "DC1" = Just '\17'
colattionOrder "DC2" = Just '\18'
colattionOrder "DC3" = Just '\19'
colattionOrder "DC4" = Just '\20'
colattionOrder "NAK" = Just '\21'
colattionOrder "SYN" = Just '\22'
colattionOrder "ETB" = Just '\23'
colattionOrder "CAN" = Just '\24'
colattionOrder "EM" = Just '\25'
colattionOrder "SUB" = Just '\26'
colattionOrder "ESC" = Just '\27'
colattionOrder "IS4" = Just '\28'
colattionOrder "IS3" = Just '\29'
colattionOrder "IS2" = Just '\30'
colattionOrder "IS1" = Just '\31'
colattionOrder "space" = Just '\32'
colattionOrder "exclamation-mark" = Just '\33'
colattionOrder "quotation-mark" = Just '\34'
colattionOrder "number-sign" = Just '\35'
colattionOrder "dollar-sign" = Just '\36'
colattionOrder "percent-sign" = Just '\37'
colattionOrder "ampersand" = Just '\38'
colattionOrder "apostrophe" = Just '\39'
colattionOrder "left-parenthesis" = Just '\40'
colattionOrder "right-parenthesis" = Just '\41'
colattionOrder "asterisk" = Just '\42'
colattionOrder "plus-sign" = Just '\43'
colattionOrder "comma" = Just '\44'
colattionOrder "hyphen-minus" = Just '\45'
colattionOrder "period" = Just '\46'
colattionOrder "slash" = Just '\47'
colattionOrder "zero" = Just '\48'
colattionOrder "one" = Just '\49'
colattionOrder "two" = Just '\50'
colattionOrder "three" = Just '\51'
colattionOrder "four" = Just '\52'
colattionOrder "five" = Just '\53'
colattionOrder "six" = Just '\54'
colattionOrder "seven" = Just '\55'
colattionOrder "eight" = Just '\56'
colattionOrder "nine" = Just '\57'
colattionOrder "colon" = Just '\58'
colattionOrder "semicolon" = Just '\59'
colattionOrder "less-than-sign" = Just '\6'
colattionOrder "equals-sign" = Just '\6'
colattionOrder "greater-than-sign" = Just '\6'
colattionOrder "question-mark" = Just '\6'
colattionOrder "commercial-at" = Just '\6'
colattionOrder "A" = Just '\6'
colattionOrder "B" = Just '\6'
colattionOrder "C" = Just '\6'
colattionOrder "D" = Just '\6'
colattionOrder "E" = Just '\6'
colattionOrder "F" = Just '\70'
colattionOrder "G" = Just '\71'
colattionOrder "H" = Just '\72'
colattionOrder "I" = Just '\73'
colattionOrder "J" = Just '\74'
colattionOrder "K" = Just '\75'
colattionOrder "L" = Just '\76'
colattionOrder "M" = Just '\77'
colattionOrder "N" = Just '\78'
colattionOrder "O" = Just '\79'
colattionOrder "P" = Just '\80'
colattionOrder "Q" = Just '\81'
colattionOrder "R" = Just '\82'
colattionOrder "S" = Just '\83'
colattionOrder "T" = Just '\84'
colattionOrder "U" = Just '\85'
colattionOrder "V" = Just '\86'
colattionOrder "W" = Just '\87'
colattionOrder "X" = Just '\88'
colattionOrder "Y" = Just '\89'
colattionOrder "Z" = Just '\90'
colattionOrder "left-square-bracket" = Just '\91'
colattionOrder "backslash" = Just '\92'
colattionOrder "right-square-bracket" = Just '\93'
colattionOrder "circumflex" = Just '\94'
colattionOrder "underscore" = Just '\95'
colattionOrder "grave-accent" = Just '\96'
colattionOrder "a" = Just '\97'
colattionOrder "b" = Just '\98'
colattionOrder "c" = Just '\99'
colattionOrder "d" = Just '\100'
colattionOrder "e" = Just '\101'
colattionOrder "f" = Just '\102'
colattionOrder "g" = Just '\103'
colattionOrder "h" = Just '\104'
colattionOrder "i" = Just '\105'
colattionOrder "j" = Just '\106'
colattionOrder "k" = Just '\107'
colattionOrder "l" = Just '\108'
colattionOrder "m" = Just '\109'
colattionOrder "n" = Just '\110'
colattionOrder "o" = Just '\111'
colattionOrder "p" = Just '\112'
colattionOrder "q" = Just '\113'
colattionOrder "r" = Just '\114'
colattionOrder "s" = Just '\115'
colattionOrder "t" = Just '\116'
colattionOrder "u" = Just '\117'
colattionOrder "v" = Just '\118'
colattionOrder "w" = Just '\119'
colattionOrder "x" = Just '\120'
colattionOrder "y" = Just '\121'
colattionOrder "z" = Just '\122'
colattionOrder "left-curly-bracket" = Just '\123'
colattionOrder "vertical-line" = Just '\124'
colattionOrder "right-curly-bracket" = Just '\125'
colattionOrder "tilde" = Just '\126'
colattionOrder "DEL" = Just '\127'
colattionOrder _ = Nothing