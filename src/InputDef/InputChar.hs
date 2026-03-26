module InputDef.InputChar where

data InputChar = InputChar {
    value :: Char,
    position :: ((Int, Int), FilePath)
}

newtype FilePos = FilePos (Int, Int)

instance Show FilePos where
    show (FilePos (ln, cn)) = show ln ++ (':' : show cn)

instance Show InputChar where
    show (InputChar val (pos, path)) = [val]

getPoseString :: (Int, Int) -> String
getPoseString (a, b) = show a ++ ':' : show b

toString :: [InputChar] -> String
toString [] = []
toString ((InputChar x _): xs) = x : toString xs

type InputString = [InputChar]

