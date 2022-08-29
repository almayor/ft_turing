 {-# LANGUAGE NamedFieldPuns #-}

module Engine (runEngine) where

import Prelude hiding (read)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import Prettyprinter

import Types
import Tape
import Data.List (find)
import System.IO (hPutStrLn, stderr)

getTransition :: Engine Transition
getTransition = do
    MachineState {tape, stateName} <- get
    Specification {transitions}    <- ask
    let c = focus tape
    let result = M.lookup stateName transitions
             >>= find (\(Transition {read = c'}) -> c' == c)
    maybe (throwError $ "No transition given for (" ++ stateName ++ ", " ++ c ++ ")")
        return result

logTransition :: StateName -> Transition -> Engine ()
logTransition stateName0 (Transition c0 c1 stateName1 action) = do
    MachineState {tape, stats} <- get
    let Stats {minIndex, maxIndex} = stats
    liftIO . print $ pretty (sliceTape minIndex maxIndex tape)
                 <+> pretty (stateName0, c0)
                 <+> pretty "->"
                 <+> pretty (stateName1, c1, action)

next :: Engine ()
next = do
    MachineState {tape = tape0, stateName = stateName0, stats} <- get
    specif <- ask
    when (isStuck specif stats) $ throwError "Machine has stuck"
    transition@(Transition _ c1 stateName1 action) <- getTransition
    logTransition stateName0 transition
    let tape' = writeTape c1 tape0
    let tape1 = case action of
            LEFT  -> moveL tape'
            RIGHT -> moveR tape'
    put $ MachineState tape1 stateName1 stats {
        nSteps   = nSteps stats + 1,
        minIndex = min (minIndex stats) (index tape1),
        maxIndex = max (maxIndex stats) (index tape1)
    }

    where
        isStuck Specification {states, alphabet}
                Stats {nSteps, minIndex, maxIndex} =
            let nStates  = fromIntegral $ length states
                nSymbols = fromIntegral $ length alphabet
            in nSteps >= nStates * nSymbols * (maxIndex - minIndex)

engine :: Engine ()
engine = do
    halted <- hasHalted
    if halted then return () else next >> engine
    where
        hasHalted :: Engine Bool
        hasHalted = do
            MachineState {stateName} <- get
            Specification {finals}   <- ask
            return $ stateName `elem` finals

runEngine :: Specification -> [Symbol] -> IO ()
runEngine specif@(Specification {blank, initial}) program = do
    let engine' = (liftIO . print $ pretty specif) >> engine
    runExceptT $ evalStateT (runReaderT engine' specif) initState
    >>= either (hPutStrLn stderr) return
    where
        initState :: MachineState
        initState =
            let initTape  = makeTape blank program
                initStats = Stats 
                    { nSteps = 0
                    , minIndex = -5
                    , maxIndex = fromIntegral $ length program + 5 }
            in MachineState initTape initial initStats
