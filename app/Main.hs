 {-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Prelude hiding (read)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import System.Exit (die)
import Control.Monad.Except
import Options.Applicative

import Types
import Engine

argparse :: IO (String, String)
argparse = customExecParser (prefs showHelpOnEmpty) opts
    where
        opts = info (args <**> helper) briefDesc
        args = (,)
            <$> argument str
                ( metavar "json"
               <> help "json description of the machine" )
            <*> argument str
                ( metavar "input"
               <> help "input of the machine" )

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
    (configFile, input) <- argparse
    description <- B.readFile configFile
    let program = map Symbol input
    let specification = eitherDecode description >>= validate program
    case specification of 
        Left msg     -> die msg
        Right specif -> runEngine specif program
