module ParserDef.InputChar where

data InputChar = InputChar {
    value :: Char,
    position :: ((Int, Int), FilePath)
}

newtype FilePos = FilePos (Int, Int)

instance Show FilePos where
    show (FilePos (ln, cn)) = show ln ++ (':' : show cn)

instance Show InputChar where
    show (InputChar val (pos, path)) = [val]

getPose :: (Int, Int) -> String
getPose (a, b) = show a ++ ':' : show b

type InputString = [InputChar]

