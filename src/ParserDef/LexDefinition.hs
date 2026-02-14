module ParserDef.LexDefinition where 

-- Atoms
type Regex     = String
type Action    = String
type CodeBlock = String
type MacroName = String

-- Whole file
data LexFile = LexFile
    { definitions    :: [Definition]
    , rules          :: [Rule]
    , userSubroutine :: String
    }

instance Show LexFile where
    show (LexFile d r u) = 
        "Def   -- \n" ++ unlines (map (("\t" ++). show) d) ++ "\n" ++
        "Rules -- \n" ++ unlines (map (("\t" ++). show) r) ++ "\n"  
        ++ "SubRo -- \n" ++ u ++ "\n -- -- --"


printList :: Show a => [a] -> IO()
printList (x:xs) = do
    print x
    printList xs
printList [] = return ()

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
