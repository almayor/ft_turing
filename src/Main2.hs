{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Main2 where

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

bla :: IO ()
bla = run `catchError` handler
    where
    handler e = hPrint stderr e >> exitFailure
    run = do
        args <- getArgs
        let helpRequested = "--help" `elem` args || "-h" `elem` args
        when (length args /= 2 || helpRequested) printUsage
        let fName   = args !! 0
        let program = map (:[]) (args !! 1)
        print program
        contents <- B.readFile fName
        case eitherDecode contents of
            Left e -> handler e
            Right s -> do
                result <- runEngine s program
                case result of
                    Left e -> handler e
                    Right _ -> exitSuccess
        -- let specifE = eitherDecode contents  :: Either String Specification
        -- let specifE' = specifE >>= validate
            -- runEngine specif' (map (:[]) program)
        -- either (hPutStrLn stderr) (c program) $
        --     eitherDecode contents >>= validate
            -- eitherDecode contents >>= validate
        -- eitherDecode contents >>= \specif -> runEngine specif program
        -- do
            -- specif <- eitherDecode contents :: Either String Specification
        -- either putError return result
        -- either putError ()
        -- case eitherDecode contents >>= validate :: Either String Specification
        -- either putError $ eitherDecode contents :: IO Specification
        --         -- (eitherDecode contents :: Either String Specification) :: IO Specification
        -- exitSuccess
