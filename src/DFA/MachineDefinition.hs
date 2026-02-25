module DFA.MachineDefinition where

data Node = Node {
    tr :: [(Char, Node)],
    isTerminal :: Bool
} deriving Show

data LexInformation = LexInformation {
    defCode :: String,
    regexCode :: String,
    regexAction :: (String, String),
    userCode :: String
}


