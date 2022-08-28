{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)

import Tape

type StateName = String
type Symbol = String

data Action = LEFT | RIGHT
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Transition = Transition {
    read        :: Symbol,
    write       :: Symbol,
    to_state    :: StateName,
    action      :: Action
}  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Specification = Specification {
    name        :: String,
    alphabet    :: [Symbol],
    blank       :: Symbol,
    states      :: [StateName],
    initial     :: StateName,
    finals      :: [StateName],
    transitions :: Map StateName [Transition]
}  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type MachineState = (Tape Symbol, StateName)
