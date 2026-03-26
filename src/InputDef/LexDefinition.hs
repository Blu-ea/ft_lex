module InputDef.LexDefinition where 
import InputDef.InputChar ( InputChar )

-- Atoms
type Regex      = [InputChar]
type Action     = String
type CodeBlock  = String
type MacroName  = String
type MRegex     = String

-- Whole file
data LexFile = LexFile
    { definitions    :: [Definition]
    , rules          :: [Rule]
    , userSubroutine :: String
    }

instance Show LexFile where
    show (LexFile d r u) = 
        "Def   -- \n" ++ unlines (map (("\t" ++). show) d) ++ "\n" ++
        "Rules -- \n" ++ unlines (map (("\t" ++). show) r) ++ "\n" ++
        "SubRo -- \n" ++ u ++ "\n -- -- --"


printList :: Show a => [a] -> IO()
printList (x:xs) = do
    print x
    printList xs
printList [] = return ()

data Definition
    = Macro MacroName MRegex
    | DCode CodeBlock
    | Array
    | Pointer
    deriving Show

getDefinition :: String -> [Definition] -> Maybe String
getDefinition _ [] = Nothing
getDefinition word ((Macro key value):envs)
    | key == word = Just value
    | otherwise = getDefinition word envs
getDefinition word (_: envs) = getDefinition word envs


data Rule
    = RRule Regex Action
    | RCode CodeBlock

instance Show Rule where
    show (RRule regex action) = 
        "RRule -> " ++ show regex ++ " <|> " ++ show action
    show (RCode code) = 
        "RCode -> " ++ show code
