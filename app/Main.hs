 {-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Types
import Engine

import Prelude hiding (read)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hPrint)
import System.Environment (getArgs, getProgName)
import Control.Monad.Except



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

    unless (initial `elem` states) $ Left "Initial state must be a state"
    unless (all (`elem` states) finals) $ Left "Each final state must be a state"
    unless (blank `elem` alphabet) $ Left "Blank symbol must be in the alphabet"

    let check ((state0, _) :-> _) = state0 `elem` states in
        unless (all check $ M.elems transitions) $ Left "Each transition must be from a state"
    let check (_ :-> (state1, _, _)) = state1 `elem` states in
        unless (all check $ M.elems transitions) $ Left "Each transition must be to a state"
    let check ((_, c0) :-> (_, c1, _)) = c0 `elem` alphabet && c1 `elem` alphabet in
        unless (all check $ M.elems transitions) $ Left "Transition symbols must be in the alphabet"

    unless (all (`elem` alphabet) program) $ Left "Invalid program"
    return specif

main :: IO ()
main = do
    args <- getArgs
    let helpRequested = "--help" `elem` args || "-h" `elem` args
    when (length args /= 2 || helpRequested) printUsage

    description <- B.readFile $ head args
    let program = map Symbol $ args !! 1
    let specification = eitherDecode description >>= validate program
    case specification of 
        Left msg     -> hPutStrLn stderr msg >> exitFailure
        Right specif -> runEngine specif program
