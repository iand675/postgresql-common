module Lib
    ( someFunc
    ) where

import Data.Word

data LSN = LSN !Word32 !Word32
  deriving (Show, Eq)
