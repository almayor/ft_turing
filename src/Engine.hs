module Engine where

import Prelude hiding (read)

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M

import Types
import Tape
import Data.List (find)

type TapeS = Tape Symbol

data Stats = Stats {
    nSteps   :: Integer,
    minIndex :: Integer,
    maxIndex :: Integer,
    transLog :: [(StateName, TapeS, Transition)]
}  deriving (Eq, Show)


data MachineState = MState TapeS StateName Stats
type Engine a = ReaderT Specification (StateT MachineState (ExceptT String Identity)) a

stats0 :: Stats
stats0 = Stats {
    nSteps   = 0,
    minIndex = 0,
    maxIndex = 0,
    transLog = []
}

getTrans :: Engine Transition
getTrans = do
    spec                <- ask
    MState tape sname _ <- get
    let c = focus tape
    maybe (throwError "No transition for current state") return $
        M.lookup sname (transitions spec)
        >>= find (\t -> read t == c)
    
makeTrans :: Transition -> Engine ()
makeTrans trans = do
    MState tape sname stats <- get
    let c      = write trans
    let tape'  = writeTape c tape
    let sname' = to_state trans
    let tape'' = case action trans of
            LEFT  -> moveLeft tape'
            RIGHT -> moveRight tape'
    put $ MState tape'' sname' stats

updateStats :: Engine ()
updateStats = do
    MState tape sname stats <- get
    put $ MState tape sname $ stats {
        nSteps   = nSteps stats + 1,
        minIndex = min (minIndex stats) (index tape),
        maxIndex = max (maxIndex stats) (index tape)
    }

stepEngine :: Engine ()
stepEngine = getTrans >>= makeTrans >> updateStats

startEngine :: Engine ()
startEngine = do
    spec <- ask
    let tape0  = blankTape (blank spec)
    let state0 = initial spec
    put $ MState tape0 state0 stats0

runEngine :: Engine ()
runEngine = do
    startEngine
    stepEngine




