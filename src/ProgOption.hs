module ProgOption where 

import System.Console.GetOpt


data Options = Options
    { optOutput     :: Bool 
    , optTest       :: Bool
    , optShowHelp   :: Bool
    } deriving Show

defaultOption :: Options
defaultOption = Options
    { optOutput     = False
    , optTest       = True
    , optShowHelp   = False
    }


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['t'] []
        (NoArg (\ opts -> opts { optOutput = True }))
        "Output the file in StdOut instead of `lex.yy.c`"
    , Option [] ["test"]
        (NoArg (\ opts -> opts { optTest = True }))
        "Just a test opt"
    , Option ['h'] ["help"]
        (NoArg (\ opts -> opts { optShowHelp = True }))
        "Show The help Menue"
    ]

processOpts :: [String] -> Either String (Options, [FilePath]) 
processOpts argv = 
    case getOpt Permute options argv of
        (opt, nopt, []) -> return (foldl (flip id) defaultOption opt, nopt)
        (_, _, err) -> Left $ concat err ++ "\n" ++ usageString

usageString :: String
usageString = usageInfo header options
    where header = "Usage: ic [OPTION...] files..."

printUsage :: IO()
printUsage = putStr usageString