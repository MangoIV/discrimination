{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Criterion.Main
import Criterion.Types
import Data.Discrimination (nub, sort)
import qualified Data.List as L
import qualified System.Random.SplitMix as SM
import Utils

main :: IO ()
main = do
  let gen = SM.mkSMGen 0xc0de

  let xs100 = finiteList gen 100
  let xs1000 = finiteList gen 1000
  let xs10000 = finiteList gen 10000

  evaluate $ rnf xs100
  evaluate $ rnf xs1000
  evaluate $ rnf xs10000

  lists <-
    traverse
      ( \i -> do
          let l = finiteListOfLists i
          evaluate $ rnf l
          pure (show i, whnf (length . sort) l)
      )
      [1_000, 2_000, 50_000]

  defaultMainWith
    (defaultConfig {timeLimit = 1})
    [ bgroup "sort with unbounded" $
        map (uncurry bench) lists
    ]
