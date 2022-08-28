module Engine where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import Types

type Engine = ReaderT Specification (StateT MachineState (ExceptT String Identity)) Int




