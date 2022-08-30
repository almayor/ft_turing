{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}

module Types where

import Prelude hiding (read)
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Map (Map, toList)
import Data.Semigroup (stimes)
import Control.Monad.Except ( ExceptT )
import Control.Monad.State ( StateT )
import Control.Monad.Reader ( ReaderT )
import Prettyprinter

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

instance Pretty Action where
    pretty = pretty . show

instance Pretty Specification where
    pretty specif = vsep
        [ stimes w ast
        , ast <> stimes (w - 2) space <> ast
        , printName
        , ast <> stimes (w - 2) space <> ast
        , stimes w ast
        , pretty "Alphabet:" <+> prettyList' (alphabet specif)
        , pretty "States:"   <+> prettyList' (states specif)
        , pretty "Initial:"  <+> pretty (initial specif)
        , pretty "Finals:"   <+> prettyList' (finals specif)
        , pretty "Transitions:"
        , indent 2 . vsep . map printTransitions . toList $ transitions specif
        , stimes w ast ]
        
        where
            ast = pretty '*'
            w = 80 :: Int
            prettyList' = encloseSep (pretty "[ ") (pretty " ]") (pretty ", ")
                        . map pretty
            printTransition from_state
                (Transition {read, write, to_state, action}) =
                    pretty (from_state, read)
                <+> pretty "->"
                <+> pretty (to_state, write, action)
            printTransitions (stateName, transitions) =
                vsep $ map (printTransition stateName) transitions
            printName =
                let nameWidth = length (name specif)
                    pad = (w - nameWidth) `div` 2
                in ast
                <> fill (w - 2) (stimes pad space <> pretty (name specif))
                <> ast

data MachineState = MachineState {
    tape      :: Tape Symbol,
    stateName :: StateName,
    stats     :: Stats
}

data Stats = Stats {
    nSteps    :: Integer,
    minIndex  :: Integer,
    maxIndex  :: Integer
}  deriving (Eq, Show)

type Engine a = ReaderT Specification (StateT MachineState (ExceptT String IO)) a


        

       

