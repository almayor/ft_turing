module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment

import Types
import Engine

import Control.Monad.Except
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, hPrint)
import Checker

printUsage :: IO a
printUsage = do
    progName <- getProgName
    hPutStrLn stderr $ 
        "Usage: " ++ progName ++ " [-h] jsonfile input\n\
        \\n\
        \positional arguments:\n\
        \jsonfile              json description of the machine\n\
        \\n\
        \input                 input of the machine\n\
        \\n\
        \optional arguments:\n\
        \-h, --help            show this help message and exit"
    exitFailure

main :: IO ()
main = run `catchError` handler
    where
    handler e = hPrint stderr e >> exitFailure
    run = do
        args <- getArgs
        let helpRequested = "--help" `elem` args || "-h" `elem` args
        when (length args /= 2 || helpRequested) printUsage
        
        let program = map (:[]) (args !! 1)
        description <- B.readFile $ head args
        let specification = eitherDecode description >>= validate
        case specification of 
            Left msg     -> hPutStrLn stderr msg >> exitFailure
            Right specif -> runEngine specif program
