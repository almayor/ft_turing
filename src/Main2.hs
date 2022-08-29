 {-# LANGUAGE NamedFieldPuns #-}
module Main2 where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment

import Types
import Engine

import Prelude hiding (read)
import Control.Monad.Except
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hPrint)
import qualified Data.Map as M
-- import Checker

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

validate :: [Symbol] -> Specification -> Either String Specification
validate program specif@(Specification{name, alphabet, blank, states,
                                       initial, finals, transitions}) = do
                                
    when (null name) $ Left "Program name cannot be empty"
    when (null initial) $ Left "There must be an initial state"
    when (null finals) $ Left "There must be at least one final state"
    when (null transitions) $ Left "There must be at least one transition"
    when (null alphabet) $ Left "There must be at least one symbol in the alphabet"
    when (null states) $ Left "There must be at least one state"
    when (null blank) $ Left "There must be a blank symbol in the alphabet"

    unless (initial `elem` states) $ Left "Initial state must be a state"
    unless (all (`elem` states) finals) $ Left "Each final state must be a state"
    unless (blank `elem` alphabet) $ Left "Blank symbol must be in the alphabet"
    unless (all (`elem` states) $ M.keys transitions) $ Left "Each transition must be from a state"
    unless (all (`elem` alphabet) program) $ Left "Invalid program"

    let check (Transition {to_state}) = to_state `elem` states in
        unless (all (all check) $ M.elems transitions) $ Left "Each transition must be to a state"
    let check (Transition {read, write}) = read `elem` alphabet && write `elem` alphabet in
        unless (all (all check) $ M.elems transitions) $ Left "Transition symbols must be in the alphabet"

    return specif

bla :: IO ()
bla = run `catchError` handler
    where
    handler e = hPrint stderr e >> exitFailure
    run = do
        args <- getArgs
        let helpRequested = "--help" `elem` args || "-h" `elem` args
        when (length args /= 2 || helpRequested) printUsage

        description <- B.readFile $ head args
        let program = map (:[]) (args !! 1) :: [Symbol]
        let specification = eitherDecode description >>= validate program
        case specification of 
            Left msg     -> hPutStrLn stderr msg >> exitFailure
            Right specif -> runEngine specif program
