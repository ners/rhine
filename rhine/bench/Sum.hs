{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

{- | Sums up natural numbers.

First create a lazy list [0, 1, 2, ...] and then sum over it.
-}
module Sum where

import "base" Data.Functor.Identity

import "criterion" Criterion.Main

import "dunai" Data.MonadicStreamFunction as Dunai

import Data.Automaton.Result (Result (..))
import "rhine" Data.Automaton as Automaton (AutomatonT (..))
import "rhine" Data.Automaton.Optimized (OptimizedAutomatonT (Stateful))
import "rhine" FRP.Rhine as Rhine

nMax :: Int
nMax = 1_000_000

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Sum"
    [ bench "rhine" $ nf rhine nMax
    , bench "dunai" $ nf dunai nMax
    , bench "automaton" $ nf automaton nMax
    , bench "foldl'" $ nf direct nMax
    ]

rhine :: Int -> Int
rhine n = sum $ runIdentity $ Rhine.embed Rhine.count $ replicate n ()

dunai :: Int -> Int
dunai n = sum $ runIdentity $ Dunai.embed Dunai.count $ replicate n ()

automaton :: Int -> Int
automaton n = sum $ runIdentity $ Rhine.embed myCount $ replicate n ()
  where
    myCount :: Rhine.MSF Identity () Int
    myCount =
      MSF $
        Stateful
          AutomatonT
            { state = 1
            , Automaton.step = \s -> return $! Result (s + 1) s
            }

direct :: Int -> Int
direct n = sum [0 .. n]
