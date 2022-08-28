import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment

import Lib
import Types
import Engine

import Control.Monad.Except

main :: IO ()
main = run `catchError` handler where
    handler e = putStrLn $ "Error: " ++ show e
    run = do
        args <- getArgs
        contents <- B.readFile (head args)
        let specif = eitherDecode contents :: Either String Specification
        print specif
        -- maybe (putStrLn "couldn't parse") print specif 


