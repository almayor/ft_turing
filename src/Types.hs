{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Prelude hiding (read)
import GHC.Generics
import Data.Aeson
import Data.Map (Map, toList)
import Data.Semigroup (stimes)

import Prettyprinter

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
    pretty spec = vsep
        [ printName
        , pretty "Alphabet:" <+> prettyList' (alphabet spec)
        , pretty "States:" <+> prettyList' (states spec)
        , pretty "Initial:" <+> pretty (initial spec)
        , pretty "Finals:" <+> prettyList' (finals spec)
        , pretty "Transitions:"
        , indent 2 . vsep . map printTransitions . toList $ transitions spec ]
        where
        prettyList' = encloseSep' "[ " " ]" ", " . map pretty
        encloseSep' a b c = encloseSep (pretty a) (pretty b) (pretty c)
        printTransition from_state (Transition {read, write, to_state, action}) =
                pretty (from_state, read)
            <+> pretty "->"
            <+> pretty (to_state, write, action)
        printTransitions (state, transitions) =
            vsep $ map (printTransition state) transitions
        printName =
            let ast = pretty '*'
                w = 80 :: Int
                nameWidth = length (name spec)
                pad = (w - nameWidth) `div` 2
            in vsep
            [ stimes w ast
            , ast <> fill (w - 2) (stimes pad space <> pretty (name spec)) <> ast
            , stimes w ast ]
            

        

       

