{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Types where

import Prelude hiding ( read )
import GHC.Generics ( Generic )
import Data.Aeson
import Data.Map ( Map, toList )
import Data.Semigroup ( stimes )
import qualified Data.Map as M
import Data.Text ( unpack )
import Data.Aeson.Types (Parser)
import Data.Functor ( (<&>) )
import Data.Aeson.Key ( fromString )
import Prettyprinter

type StateName = String

newtype Symbol = Symbol Char
    deriving newtype (Eq, Ord, Show, Pretty)

data Action = LEFT | RIGHT
    deriving (Eq, Show, Generic, FromJSON)

data Transition = (StateName, Symbol) :-> (StateName, Symbol, Action)
    deriving (Eq)

data Specification = Specification {
    name        :: String,
    alphabet    :: [Symbol],
    blank       :: Symbol,
    states      :: [StateName],
    initial     :: StateName,
    finals      :: [StateName],
    transitions :: Map (StateName, Symbol) Transition
}  deriving (Eq, Generic)

instance Pretty Action where
    pretty = pretty . show

instance Pretty Transition where
    pretty (from :-> to) = pretty from <+> pretty "->" <+> pretty to

instance Pretty Specification where
    pretty specif =  vsep
        [ stimes w ast
        , ast <> stimes (w - 2) space <> ast
        , printName
        , ast <> stimes (w - 2) space <> ast
        , stimes w ast
        , pretty "Alphabet:"    <+> prettyList' (alphabet specif)
        , pretty "States:"      <+> prettyList' (states specif)
        , pretty "Initial:"     <+> pretty (initial specif)
        , pretty "Finals:"      <+> prettyList' (finals specif)
        , pretty "Transitions:" <+> prettyList (M.elems $ transitions specif)
        , stimes w ast
        ]
        where
            w = 80
            ast = pretty '*'
            prettyList' :: Pretty a => [a] -> Doc ann
            prettyList' = align
                        . encloseSep (pretty "[ ") (pretty " ]") (pretty ", ")
                        . map pretty
            printName =
                let nameWidth = length (name specif)
                    pad = (w - nameWidth) `div` 2
                in ast
                <> fill (w - 2) (stimes pad space <> pretty (name specif))
                <> ast

instance FromJSON Symbol where
    parseJSON (String s) = case unpack s of
        [c] -> pure $ Symbol c
        _   -> fail "Each symbol must be a single character"

    parseJSON _ = fail "Each symbol must be a single character"

data JSONTransition = JSONTransition {
    read        :: Symbol,
    write       :: Symbol,
    to_state    :: StateName,
    action      :: Action
}  deriving (Eq, Show, Generic, FromJSON)

instance FromJSON Specification where
    parseJSON = withObject "Specification" $ \v -> do
        trs' <- v .: fromString "transitions"
                                    :: Parser (Map StateName [JSONTransition])
        let trs = M.fromList $ concat $ toList trs'
               <&> \(state0, ts) -> ts
               <&> \(JSONTransition c0 c1 state1 act) ->
                    ( (state0, c0)
                    , (state0, c0) :-> (state1, c1, act)
                    )
        Specification <$> v .: fromString "name"
                      <*> v .: fromString "alphabet"
                      <*> v .: fromString "blank"
                      <*> v .: fromString "states"
                      <*> v .: fromString "initial"
                      <*> v .: fromString "finals"
                      <*> return trs
