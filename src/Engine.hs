{-# LANGUAGE NamedFieldPuns #-}
module Engine (runEngine) where

import Prelude hiding (read)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import System.Exit (die)
import Prettyprinter
import Prettyprinter.Util (putDocW)

import Types
import Tape

data MachineState = MachineState {
    tape      :: Tape Symbol,
    stateName :: StateName,
    stats     :: Stats
}

data Stats = Stats {
    nSteps    :: Integer,
    minIndex  :: Integer,
    maxIndex  :: Integer
}

type Engine a = ReaderT Specification (StateT MachineState (ExceptT String IO)) a

next :: Engine ()
next = do
    MachineState {tape = tape0, stateName = state0} <- get
    Specification {transitions} <- ask
    let c0 = focus tape0
    tr@(_ :-> (state1, c1, act)) <- case M.lookup (state0, c0) transitions of
        Just tr -> return tr
        Nothing -> throwError $ "No transition specified for (" ++ state0 ++ ", " ++ show [c0] ++ ")"
    liftIO . putDocW 160 $ pretty tr <> line

    let tape' = writeTape c1 tape0
    let tapeM = case act of
            LEFT  -> moveL tape'
            RIGHT -> moveR tape'
    tape1 <- maybe (throwError "Moved past leftmost position on tape") return tapeM

    Stats {nSteps, minIndex, maxIndex} <- gets stats
    put $ MachineState tape1 state1 $ Stats {
        nSteps   = nSteps + 1,
        minIndex = min minIndex (index tape1),
        maxIndex = max maxIndex (index tape1)
    }

engine :: Engine ()
engine = do
    Stats {nSteps, minIndex, maxIndex} <- gets stats
    tape <- gets tape
    let tapeSlice = sliceTape minIndex maxIndex tape
    liftIO . putDocW 160 $ fill 3 (pretty nSteps) <+> pretty tapeSlice <+> space

    stuck  <- hasStuck
    when stuck $ throwError "Machine has stuck"
    halted <- hasHalted
    if halted then return () else next >> engine

    where
        hasHalted :: Engine Bool
        hasHalted = do
            MachineState {stateName} <- get
            Specification {finals}   <- ask
            return $ stateName `elem` finals

        hasStuck :: Engine Bool
        hasStuck = do
            Stats {nSteps, minIndex, maxIndex} <- gets stats
            Specification {states, alphabet}   <- ask
            let nStates  = fromIntegral $ length states
            let nSymbols = fromIntegral $ length alphabet
            return $ nSteps >= nStates * nSymbols * (maxIndex - minIndex)

runEngine :: Specification -> [Symbol] -> IO ()
runEngine specif@(Specification {blank, initial}) program =
    print (pretty specif)
    >>  runExceptT (execStateT (runReaderT engine specif) initState)
    >>= either die logFinalState
    where
        initState :: MachineState
        initState =
            let tape0  = makeTape blank program
                stats0 = Stats
                    { nSteps = 0
                    , minIndex = 0
                    , maxIndex = fromIntegral $ length program + 5 }
            in  MachineState tape0 initial stats0

        logFinalState :: MachineState -> IO ()
        logFinalState finalState =
            let Stats {nSteps, minIndex, maxIndex} = stats finalState
            in  print . pretty $ "\n\nUsed: " ++ show nSteps ++ " steps, "
                               ++ show (maxIndex - minIndex) ++ " tape cells"
