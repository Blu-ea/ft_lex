module Error where
    

formatErr :: (Int, String) -> String -> String
formatErr (line, file) msg = concat [file, ":", show line, ": ", msg]
