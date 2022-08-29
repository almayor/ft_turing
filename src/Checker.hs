module Checker where

import Types

validate :: Specification -> Either String Specification
validate = return