{-# LANGUAGE OverloadedStrings #-}

module Evaluator where 

import Types
import Data.List (nub, splitAt)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

