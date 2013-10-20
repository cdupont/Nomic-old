{-# LANGUAGE TupleSections, DeriveDataTypeable, RankNTypes, ScopedTypeVariables #-}

module Date01 where

import Prelude
import Language.Nomyx
import Data.Typeable

data Season = Spring | Summer | Fall | Winter
    deriving (Typeable, Show, Eq)

data Date = Date Int Season
    deriving (Typeable, Show, Eq)

date :: MsgVar Date
date = msgVar "Date"

