import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment

import Types
import Engine

import Prettyprinter
import Control.Monad.Except
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hPrint)
import Control.Monad.Except

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    fail $
        "Usage: " ++ progName ++ " [-h] jsonfile input\
        \\n\
        \positional arguments:\n\
        \jsonfile              json description of the machine\n\
        \\n\
        \input                 input of the machine\n\
        \\n\
        \optional arguments:\n\
        \-h, --help            show this help message and exit"

main :: IO ()
main = run `catchError` handler where
    handler e = hPrint stderr e >> exitFailure
    run = do
        args <- getArgs
        let helpRequested = "--help" `elem` args || "-h" `elem` args
        when (length args /= 2 || helpRequested) printUsage
        contents <- B.readFile (head args)
        let specif = eitherDecode contents :: Either String Specification
        either fail (print . pretty) specif


