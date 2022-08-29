{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Checker where

import Prelude hiding (read)
import Control.Monad

import Types
import qualified Data.Map as M

validate :: Specification -> Either String Specification
validate specif@(Specification{name, alphabet, blank, states,
                               initial, finals, transitions}) = do
                                
    when (null name) . Left $ "Program name cannot be empty"
    when (null initial) . Left $ "There must be an initial state"
    when (null finals) . Left $ "There must be at least one final state"
    when (null transitions) . Left $ "There must be at least one transition"
    when (null alphabet) . Left $ "There must be at least one symbol in the alphabet"
    when (null states) . Left $ "There must be at least one state"
    when (null blank) . Left $ "There must be a blank symbol in the alphabet"

    unless (initial `elem` states) . Left $ "Initial state must be a state"
    unless (all (`elem` states) finals) . Left $ "Each final state must be a state"
    unless (blank `elem` alphabet) . Left $ "Blank symbol must be in the alphabet"
    unless (all (`elem` states) $ M.keys transitions) . Left $ "Each transition must be from a state"

    let check (Transition {to_state}) = to_state `elem` states in
        unless (all (all check) $ M.elems transitions) . Left $ "Each transition must be to a state"
    let check (Transition {read, write}) = read `elem` alphabet && write `elem` alphabet in
        unless (all (all check) $ M.elems transitions) . Left $ "Transition symbols must be in the alphabet"

    return specif
